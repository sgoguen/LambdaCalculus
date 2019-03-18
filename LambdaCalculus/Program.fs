/// Based on https://opendsa-server.cs.vt.edu/ODSA/Books/PL/html/index.html#lambda-calculus

namespace LambdaCalculus

open System

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

    member this.String =
        match this with
            | Variable name -> name
            | Application (func, arg) -> sprintf "(%A %A)" func arg
            | Lambda (param, body) -> sprintf "λ%s.%A" param body

    override this.ToString() = this.String

module Expr =

    module private Expr =
        open Microsoft.FSharp.Quotations.Patterns

        /// Constructs a lambda expression from an F# quotation.
        let rec ofQuot =
            function
                | Var var ->   // bound
                    Variable var.Name
                | ValueWithName (_, _, name) ->   // free
                    Variable name
                | Application (func, arg) ->
                    Application (ofQuot func, ofQuot arg)
                | Lambda (param, body) ->
                    Lambda (param.Name, ofQuot body)
                | expr -> failwithf "Not supported: %A" expr

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
                    if name = oldName then Variable newName
                    else expr
                | Application (func, arg) ->
                    Application ((convert func), (convert arg))
                | Lambda (param, body) ->   // inner lambda
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

        let allVariables expr =
            let rec loop expr : seq<string> =
                seq {
                    match expr with
                        | Variable name ->
                            yield name
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
                elif occursFree param' arg then     // avoid variable capture via α-conversion
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

    /// Reduces a β-reduction expression ("β-redex").
    let betaReduction =
        function
            | Application (Lambda (param, body), arg) ->
                substitute arg param body
            | expr -> failwithf "%A is not a β-redex" expr

    let ofQuot = Expr.ofQuot
    let True = ofQuot <@@fun x y -> x@@>
    let Identity = ofQuot <@@fun x -> x@@>

module Program =

    [<EntryPoint>]
    let main argv =
        Console.OutputEncoding <- Text.Encoding.Unicode
        let f = Application (Expr.Identity, Variable "y")
        printfn "%A" f
        printfn "%A" Expr.True
        0
