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

    /// Indicates whether the given variable occurs within a lambda expression.
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
    let alphaConvert newName expr =

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
                    Lambda(param, convert body)

        if occurs newName expr then
            failwithf "New name '%s' already appears in %A" newName expr
        match expr with
            | Lambda (param, body) ->
                Lambda (newName, convert param newName body)
            | _ -> failwithf "α-conversion not supported for %A" expr

    let ofQuot = Expr.ofQuot
    let True = ofQuot <@@(fun x y -> x)@@>
    let Identity = ofQuot <@@(fun x -> x)@@>

module Program =

    [<EntryPoint>]
    let main argv =
        Console.OutputEncoding <- Text.Encoding.Unicode
        let f = Application (Expr.Identity, Variable "y")
        printfn "%A" f
        printfn "%A" Expr.True
        0
