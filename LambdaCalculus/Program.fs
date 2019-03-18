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
            | Application (func, arg) -> sprintf "%A%A" func arg
            | Lambda (param, body) -> sprintf "λ%s.%A" param body

    override this.ToString() = this.String

module Term =

    let rec ofExpr =
        function
            | Microsoft.FSharp.Quotations.Patterns.Var var ->
                Variable var.Name
            | Microsoft.FSharp.Quotations.Patterns.Application (func, arg) ->
                Application (ofExpr func, ofExpr arg)
            | Microsoft.FSharp.Quotations.Patterns.Lambda (param, body) ->
                Lambda (param.Name, ofExpr body)
            | expr -> failwithf "Not supported: %A" expr

    let True = ofExpr <@@(fun x y -> x)@@>

[<EntryPoint>]
let main argv =
    Console.OutputEncoding <- Text.Encoding.Unicode
    printfn "True: %A" Term.True
    0
