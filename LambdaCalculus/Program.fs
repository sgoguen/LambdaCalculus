/// Based on https://opendsa-server.cs.vt.edu/ODSA/Books/PL/html/index.html#lambda-calculus

namespace LambdaCalculus

open System

type Variable = string (*name*)

[<StructuredFormatDisplay("{String}")>]
type Term =
    | Variable of Variable
    | Application of (Term (*function*) * Term (*argument*))
    | Lambda of (Variable (*parameter*) * Term (*body*))

    member this.String =
        match this with
            | Variable name -> name
            | Application (func, arg) -> sprintf "(%A %A)" func arg
            | Lambda (param, body) -> sprintf "λ%s.%A" param body

    override this.ToString() = this.String

module Term =

    module private Expr =
        open Microsoft.FSharp.Quotations.Patterns
        let rec ofExpr =
            function
                | Var var ->   // bound
                    Variable var.Name
                | ValueWithName (_, _, name) ->
                    Variable name
                | Application (func, arg) ->
                    Application (ofExpr func, ofExpr arg)
                | Lambda (param, body) ->
                    Lambda (param.Name, ofExpr body)
                | expr -> failwithf "Not supported: %A" expr

    let rec occursFree name =
        function
            | Variable name' ->
                name' = name
            | Application (func, arg) ->
                occursFree name func || occursFree name arg
            | Lambda (param, body) ->
                (param <> name) && occursFree name body

    let ofExpr = Expr.ofExpr
    let True = ofExpr <@@(fun x y -> x)@@>
    let Identity = ofExpr <@@(fun x -> x)@@>

module Program =

    [<EntryPoint>]
    let main argv =
        Console.OutputEncoding <- Text.Encoding.Unicode
        let f = Application (Term.Identity, Variable "y")
        printfn "%A" f
        printfn "%A" Term.True
        0
