/// Based on https://opendsa-server.cs.vt.edu/ODSA/Books/PL/html/index.html#lambda-calculus
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
    | Application of (Expr (*function*) * Expr (*argument*))

    /// E.g. "λx.y"
    | Lambda of (Variable (*parameter*) * Expr (*body*))

    /// Converts expression to string.
    member this.String =
        match this with
            | Variable name -> name
            | Application (func, arg) -> sprintf "(%A %A)" func arg
            | Lambda (param, body) -> sprintf "λ%s.%A" param body

    /// Converts expression to string.
    override this.ToString() = this.String

/// Lambda expression functions.
module Expr =

    /// Interop with F# quotations.
    module private FSharp =

        open Microsoft.FSharp.Quotations.Patterns

        /// Constructs a lambda expression from an F# quotation.
        let rec ofQuot =
            function
                | Var var -> Variable var.Name    // bound
                | ValueWithName (_, _, name) ->   // free
                    Variable name
                | Application (func, arg) ->
                    Application (ofQuot func, ofQuot arg)
                | Lambda (param, body) ->
                    Lambda (param.Name, ofQuot body)
                | expr -> failwithf "Not supported: %A" expr

    let ofQuot = FSharp.ofQuot

    module private Parse =

        open FParsec

        let private parseExpr, private parseExprRef =
            createParserForwardedToRef<Expr, unit>()

        let private parseName =
            many1Chars (satisfy (fun c ->
                Char.IsLetterOrDigit(c) && (c <> 'λ')))

        let private parseVariable =
            parseName
                |>> Variable

        let private parseApplication =
            pipe5
                (skipChar '(')
                parseExpr
                (many1 <| skipChar ' ')
                parseExpr
                (skipChar ')')
                (fun _ func _ arg _ ->
                    Application (func, arg))

        let private parseLambda =
            pipe4
                (skipAnyOf ['λ'; '^'; '\\'])
                parseName
                (skipChar '.')
                parseExpr
                (fun _ param _ body ->
                    Lambda (param, body))

        do parseExprRef :=
            choice [
                parseVariable
                parseApplication
                parseLambda
            ]

        let parse str =
            let parser = !parseExprRef .>> eof   // force consumption of entire string
            match run parser str with
                | Success (expr, _, _) -> expr
                | Failure (msg, _, _) -> failwith msg

    let parse = Parse.parse

    let toString (expr : Expr) =
        expr.ToString()

    /// Indicates whether the given variable occurs within a lambda expression (either
    /// bound or free).
    let rec occurs name =
        function
            | Variable name' ->
                name' = name
            | Application (func, arg) ->
                occurs name func || occurs name arg
            | Lambda (param, body) ->
                (param = name) || occurs name body

    /// Indicates whether the given variable occurs free within a lambda expression.
    /// (Note that it might occur both free and bound.)
    let rec occursFree name =
        function
            | Variable name' ->
                name' = name
            | Application (func, arg) ->
                occursFree name func || occursFree name arg
            | Lambda (param, body) ->
                (param <> name) && occursFree name body

    /// α-conversion.
    let alphaConvert newName lambda =

        let rec convert oldName newName expr =
            let convert = convert oldName newName
            match expr with
                | Variable name ->
                    assert(name <> newName)
                    if name = oldName then Variable newName   // rename variable
                    else expr
                | Application (func, arg) ->
                    Application ((convert func), (convert arg))
                | Lambda (param, body) ->                     // inner lambda
                    assert(param <> newName)
                    Lambda (param, convert body)

        if occurs newName lambda then
            failwithf "New name '%s' already appears in %A" newName lambda
        match lambda with
            | Lambda (param, body) ->
                Lambda (newName, convert param newName body)
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
                if name = param then arg            // replace this variable with the new expression
                else body                           // no-op
            | Application (func, arg) ->
                Application (subst func, subst arg)
            | Lambda (param', body') ->
                if param' = param then body         // no-op (don't actually substitute anything)
                elif occursFree param' arg then     // avoid variable capture
                    let allVars = allVariables body'
                    ['a' .. 'z']
                        |> Seq.map (fun c -> c.ToString())
                        |> Seq.tryFind (fun newName ->
                            not <| allVars.Contains(newName))
                        |> Option.map (fun newName ->
                            alphaConvert newName body
                                |> subst)
                        |> Option.defaultWith (fun () ->
                            failwithf "Exhausted variable names for α-conversion")
                else Lambda (param', subst body')   // substitute new expression in lambda body

    /// Reduces a β-reduction expression ("β-redex"). This is
    /// function evaluation, which "calls" the given lambda
    /// with the given argument.
    let betaReduction =
        function
            | Application (Lambda (param, body), arg) ->
                substitute arg param body
            | expr -> failwithf "%A is not a β-redex" expr

    /// Evaluates the given expression lazily (normal order).
    let rec eval expr =
        match expr with
            | Application (Lambda (param, body), arg) ->
                substitute arg param body |> eval
            | Application (func, arg) ->
                let expr' = Application (eval func, eval arg)
                if expr' = expr then expr   // avoid infinite recursion
                else eval expr'
            | Lambda (param, body) ->
                Lambda (param, eval body)
            | expr -> expr

[<AutoOpen>]
module Lang =

    let True = <@@ fun x y -> x @@> |> Expr.ofQuot
    let False = <@@ fun x y -> y @@> |> Expr.ofQuot
    let If = <@@ fun b x y -> b x y @@> |> Expr.ofQuot
    let And = sprintf "λp.λq.((p q) %A)" False |> Expr.parse
    let Or = sprintf "λp.λq.((p %A) q)" True |> Expr.parse

    let Zero =  <@@ fun f x -> x @@> |> Expr.ofQuot   // same as False
    let One =   <@@ fun f x -> f x @@> |> Expr.ofQuot
    let Two =   <@@ fun f x -> f (f x) @@> |> Expr.ofQuot
    let Three = <@@ fun f x -> f (f (f x)) @@> |> Expr.ofQuot
    let Four =  <@@ fun f x -> f (f (f (f x))) @@> |> Expr.ofQuot
    let Five =  <@@ fun f x -> f (f (f (f (f x)))) @@> |> Expr.ofQuot
    let Six =   <@@ fun f x -> f (f (f (f (f (f x))))) @@> |> Expr.ofQuot

    let Succ = <@@ fun n f x -> f ((n f) x) @@> |> Expr.ofQuot
    let Plus = <@@ fun m n f x -> (n f) ((m f) x) @@> |> Expr.ofQuot
    let Mult = <@@ fun m n f -> m (n f) @@> |> Expr.ofQuot

module Program =

    [<EntryPoint>]
    let main argv =
        Console.OutputEncoding <- Text.Encoding.Unicode
        0
