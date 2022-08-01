namespace LambdaCalculus

open System

/// A variable in a lambda expression.
type Variable = string (*name*)

/// Lambda expression.
[<StructuredFormatDisplay("{String}")>]
type Expr =

    /// E.g. "x"
    | Variable of Variable

    /// E.g. "(x y)"
    | Application of (Expr (*function*)  * Expr (*argument*) )

    /// E.g. "λx.y"
    | Lambda of (Variable (*parameter*)  * Expr (*body*) )

    /// Converts expression to string.
    member this.String =
        match this with
        | Variable name -> name
        | Application (func, arg) -> sprintf "(%A %A)" func arg
        | Lambda (param, body) -> sprintf "\%s.%A" param body

    /// Converts expression to string.
    override this.ToString() = this.String




module Expr =

    /// Converts an integer to a variable name
    let intToChar (n:int) = char(n + int('a')).ToString()

    ///  Converts a DeBruijn expression to a lambda expression.
    let fromDBExpr (exp: DBExpr) =
        let rec fromDBExpr maxP = function
            | DBVariable index -> Expr.Variable(intToChar (index + maxP))
            | DBApplication(fn, arg) ->
                let (fn) = fromDBExpr maxP fn
                let (arg) = fromDBExpr maxP arg
                Application((fn), (arg))
            | DBLambda(body) -> 
                let (body) = fromDBExpr (maxP - 1) body
                Lambda(intToChar (maxP - 1), (body))
        let maxP = exp.MaxParameter
        fromDBExpr maxP exp

    let fromInt n = 
        let e = DExpr.intToLambda n |> fromDBExpr
        e //|> fromDBExpr

    /// Interop with F# quotations.
    module private FSharp =

        open Microsoft.FSharp.Quotations.Patterns

        /// Constructs a lambda expression from an F# quotation.
        let rec ofQuot =
            function
            | Var var -> Variable var.Name // bound
            | ValueWithName (_, _, name) -> // free
                Variable name
            | Application (func, arg) -> Application(ofQuot func, ofQuot arg)
            | Lambda (param, body) -> Lambda(param.Name, ofQuot body)
            | expr -> failwithf "Not supported: %A" expr

    let ofQuot = FSharp.ofQuot

    let toString (expr: Expr) = expr.ToString()

    /// Indicates whether the given variable occurs within a lambda expression (either
    /// bound or free).
    let rec occurs name =
        function
        | Variable name' -> name' = name
        | Application (func, arg) -> occurs name func || occurs name arg
        | Lambda (param, body) -> (param = name) || occurs name body

    /// Indicates whether the given variable occurs free within a lambda expression.
    /// (Note that it might occur both free and bound.)
    let rec occursFree name =
        function
        | Variable name' -> name' = name
        | Application (func, arg) -> occursFree name func || occursFree name arg
        | Lambda (param, body) -> (param <> name) && occursFree name body

    /// α-conversion.
    let alphaConvert newName lambda =

        let rec convert oldName newName expr =
            let convert = convert oldName newName

            match expr with
            | Variable name ->
                assert (name <> newName)

                if name = oldName then
                    Variable newName // rename variable
                else
                    expr
            | Application (func, arg) -> Application((convert func), (convert arg))
            | Lambda (param, body) -> // inner lambda
                assert (param <> newName)
                Lambda(param, convert body)

        if occurs newName lambda then
            failwithf "New name '%s' already appears in %A" newName lambda

        match lambda with
        | Lambda (param, body) -> Lambda(newName, convert param newName body)
        | _ -> failwithf "α-conversion not supported for %A" lambda

    /// Replaces all occurrences of param with arg in body.
    let rec substitute arg param body =

        /// Answers the set of all variables in the given expression.
        let allVariables expr =
            let rec loop expr : seq<string> =
                seq {
                    match expr with
                    | Variable name -> yield name
                    | Application (func, arg) ->
                        yield! loop func
                        yield! loop arg
                    | Lambda (param, body) ->
                        yield param
                        yield! loop body
                }

            loop expr |> Set.ofSeq

        let subst = substitute arg param

        match body with
        | Variable name ->
            if name = param then
                arg // replace this variable with the new expression
            else
                body // no-op
        | Application (func, arg) -> Application(subst func, subst arg)
        | Lambda (param', body') ->
            if param' = param then
                body // no-op (don't actually substitute anything)
            elif occursFree param' arg then // avoid variable capture
                let allVars = allVariables body

                [ 'a' .. 'z' ]
                |> Seq.map (fun c -> c.ToString())
                |> Seq.tryFind (fun newName -> not <| allVars.Contains(newName))
                |> Option.map (fun newName -> alphaConvert newName body |> subst)
                |> Option.defaultWith (fun () -> failwithf "Exhausted variable names for α-conversion")
            else
                Lambda(param', subst body') // substitute new expression in lambda body

    /// Reduces a β-reduction expression ("β-redex"). This is
    /// function evaluation, which "calls" the given lambda
    /// with the given argument.
    let betaReduce =
        function
        | Application (Lambda (param, body), arg) -> substitute arg param body
        | expr -> failwithf "%A is not a β-redex" expr

    /// Evaluates the given expression lazily (normal order).
    /// See reduceLeftmostOutermostBetaRedex and reduceToNormalForm in
    /// https://opendsa-server.cs.vt.edu/ODSA/AV/PL/interpreters/lambdacalc/version1.4.used.in.book/scripts/interpreter.js
    let eval expr =

        let rec containsBetaRedex =
            function
            | Variable _ -> false
            | Application (Lambda (_, _), _) -> true
            | Application (func, arg) -> containsBetaRedex func || containsBetaRedex arg
            | Lambda (_, body) -> containsBetaRedex body

        let rec reduceLazy expr =
            match expr with
            | Variable _ -> expr
            | Application (Lambda (_, _), _) -> betaReduce expr
            | Application (func, arg) ->
                if containsBetaRedex func then
                    Application(reduceLazy func, arg)
                elif containsBetaRedex arg then
                    Application(func, reduceLazy arg)
                else
                    expr
            | Lambda (param, body) -> Lambda(param, reduceLazy body)

        let rec loop expr =
            if containsBetaRedex expr then
                reduceLazy expr |> loop
            else
                expr

        loop expr
