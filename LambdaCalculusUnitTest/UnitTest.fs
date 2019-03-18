namespace LambdaCalculusUnitTest

open Microsoft.VisualStudio.TestTools.UnitTesting
open LambdaCalculus
open System

[<TestClassAttribute>]
type UnitTest() =

    let subst arg param body =
        Expr.substitute
            (Expr.parse arg)
            param
            (Expr.parse body)
            |> Expr.toString

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
    member __.αConvert1() =
        let before = Expr.ofQuot <@@fun x y -> x@@>
        let after = Expr.ofQuot <@@fun z y -> z@@>
        Assert.AreEqual(after, Expr.alphaConvert "z" before)

    [<TestMethod>]
    member __.Parse1() =
        let str = "λx.y"
        Assert.AreEqual(str, str |> Expr.parse |> Expr.toString)
       
    [<TestMethod>]
    member __.Substitute1() =
        Assert.AreEqual(
            "λx.(w z)",
            subst "(w z)" "y" "λx.y")

    [<TestMethod>]
    member __.Substitute2() =
        Assert.AreEqual(
            "λa.a",
            subst "(w x)" "y" "λx.x")

    [<TestMethod>]
    member __.βReduce1() =
        let actual =
            Expr.parse "(λx.(x v) (z (v u)))"
                |> Expr.betaReduction
                |> Expr.toString
        Assert.AreEqual("((z (v u)) v)", actual)

    [<TestMethod>]
    member __.Eval1() =
        let actual =
            Expr.parse "(λx.((x y) (y x)) (λw.(w w) z))"
                |> Expr.eval
                |> Expr.toString
        Assert.AreEqual("(((z z) y) (y (z z)))", actual)
