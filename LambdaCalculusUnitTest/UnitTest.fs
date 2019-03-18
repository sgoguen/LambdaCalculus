namespace LambdaCalculusUnitTest

open Microsoft.VisualStudio.TestTools.UnitTesting
open LambdaCalculus
open System

[<TestClassAttribute>]
type UnitTest() = 

    [<TestMethod>]
    member __.ToString1() =
        Assert.AreEqual(
            "λx.λy.x",
            sprintf "%A" <| Expr.ofQuot <@@(fun x y -> x)@@>)

    [<TestMethod>]
    member __.ToString2() =
        let z = 0
        Assert.AreEqual(
            "λx.λy.z",
            sprintf "%A" <| Expr.ofQuot <@@(fun x y -> z)@@>)

    [<TestMethod>]
    member __.OccursFree1() =
        Assert.IsTrue(Expr.occursFree "x"
            <| Variable "x")

    [<TestMethod>]
    member __.OccursFree2() =
        Assert.IsFalse(Expr.occursFree "x"
            <| Variable "y")

    [<TestMethod>]
    member __.OccursFree3() =
        Assert.IsTrue(Expr.occursFree "x"
            <| Application (Variable "x", Variable "y"))

    [<TestMethod>]
    member __.OccursFree4() =
        Assert.IsFalse(Expr.occursFree "x"
            <| Expr.ofQuot <@@fun z x -> x@@>)

    [<TestMethod>]
    member __.OccursFree5() =
        let x = ()
        Assert.IsTrue(Expr.occursFree "x"
            <| Expr.ofQuot <@@fun z -> x@@>)

    [<TestMethod>]
    member __.OccursFree6() =
        let x = ()
        Assert.IsTrue(Expr.occursFree "x"
            <| Expr.ofQuot <@@(fun x -> x) x@@>)

    [<TestMethod>]
    member __.αConvert() =
        let before = Expr.ofQuot <@@fun x y -> x@@>
        let after = Expr.ofQuot <@@fun z y -> z@@>
        Assert.AreEqual(after, Expr.alphaConvert "z" before)
       
    [<TestMethod>]
    member __.Substitute1() =
        let w () = ()
        let z = ()
        let newExpr = Expr.ofQuot <@@w z@@>
        let oldExpr =
            let y = ()
            Expr.ofQuot <@@fun x -> y@@>
        let actual = oldExpr |> Expr.substitute newExpr
        let expected = Expr.ofQuot <@@fun x -> w z@@>
        Assert.AreEqual(expected, actual)
