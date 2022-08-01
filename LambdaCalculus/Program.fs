// Based on https://opendsa-server.cs.vt.edu/ODSA/Books/PL/html/index.html#lambda-calculus
namespace LambdaCalculus

open System

[<AutoOpen>]
module Lang =

    open LambdaCalculus
    open LambdaCalculus.Parser

    let True = <@@ fun x y -> x @@> |> Expr.ofQuot
    let False = <@@ fun x y -> y @@> |> Expr.ofQuot
    let If = <@@ fun b x y -> b x y @@> |> Expr.ofQuot
    let And = sprintf "λp.λq.((p q) %A)" False |> Expr.parse
    let Or = sprintf "λp.λq.((p %A) q)" True |> Expr.parse

    let Zero = <@@ fun f x -> x @@> |> Expr.ofQuot // same as False
    let One = <@@ fun f x -> f x @@> |> Expr.ofQuot
    let Two = <@@ fun f x -> f (f x) @@> |> Expr.ofQuot
    let Three = <@@ fun f x -> f (f (f x)) @@> |> Expr.ofQuot
    let Four = <@@ fun f x -> f (f (f (f x))) @@> |> Expr.ofQuot

    let Five =
        <@@ fun f x -> f (f (f (f (f x)))) @@>
        |> Expr.ofQuot

    let Six =
        <@@ fun f x -> f (f (f (f (f (f x))))) @@>
        |> Expr.ofQuot

    let Seven =
        <@@ fun f x -> f (f (f (f (f (f (f x)))))) @@>
        |> Expr.ofQuot

    let Eight =
        <@@ fun f x -> f (f (f (f (f (f (f (f x))))))) @@>
        |> Expr.ofQuot

    let Nine =
        <@@ fun f x -> f (f (f (f (f (f (f (f (f x)))))))) @@>
        |> Expr.ofQuot

    let Ten =
        <@@ fun f x -> f (f (f (f (f (f (f (f (f (f x))))))))) @@>
        |> Expr.ofQuot

    let Succ = <@@ fun n f x -> f ((n f) x) @@> |> Expr.ofQuot

    let Plus =
        <@@ fun m n f x -> (n f) ((m f) x) @@>
        |> Expr.ofQuot

    let Mult = <@@ fun m n f -> m (n f) @@> |> Expr.ofQuot

    /// Y-combinator for recursion
    let Y = "λh.(λx.(h (x x)) λx.(h (x x)))" |> Expr.parse

module Program =

    open LambdaCalculus.Parser

    [<EntryPoint>]
    let main argv =

        // display λ chars correctly
        Console.OutputEncoding <- Text.Encoding.Unicode

        let IsZero =
            sprintf "λn.((n λx.%A) %A)" False True
            |> Expr.parse

        let Pred =
            "λn.λf.λx.(((n λg.λh.(h (g f))) λu.x) λu.u)"
            |> Expr.parse

        let TriangleNonRecursive =
            sprintf "λg.λn.(((%A (%A n)) %A) ((%A n) (g (%A n))))" If IsZero Zero Plus Pred
            |> Expr.parse

        let TriangleRecursive =
            sprintf "(%A %A)" Y TriangleNonRecursive
            |> Expr.parse

        let expr =
            sprintf "(%A %A)" TriangleRecursive Four
            |> Expr.parse
            |> Expr.eval

        printfn "%A" expr

        //  Let's enumerate the first 100 terms
        for i in 0..10 do 
            let db = DExpr.intToLambda i
            let dbs = db |> DExpr.toString
            let o = DExpr.lambdaToInt db
            let e = Expr.fromDBExpr db
            printfn "%i - %i - %A - %s" i o e dbs

        0
