namespace LambdaCalculusUnitTest

open Microsoft.VisualStudio.TestTools.UnitTesting
open LambdaCalculus

[<TestClassAttribute>]
type UnitTest() = 

    [<TestMethod>]
    member __.ToString1() =
        Assert.AreEqual(
            "λx.λy.x",
            sprintf "%A" <| Term.ofExpr <@@(fun x y -> x)@@>)

    [<TestMethod>]
    member __.ToString2() =
        let z = 0
        Assert.AreEqual(
            "λx.λy.z",
            sprintf "%A" <| Term.ofExpr <@@(fun x y -> z)@@>)

    [<TestMethod>]
    member __.OccursFree1() =
        Assert.IsTrue(Term.occursFree "x"
            <| Variable "x")

    [<TestMethod>]
    member __.OccursFree2() =
        Assert.IsFalse(Term.occursFree "x"
            <| Variable "y")

    [<TestMethod>]
    member __.OccursFree3() =
        Assert.IsTrue(Term.occursFree "x"
            <| Application (Variable "x", Variable "y"))

    [<TestMethod>]
    member __.OccursFree4() =
        Assert.IsFalse(Term.occursFree "x"
            <| Term.ofExpr <@@(fun z x -> x)@@>)

    [<TestMethod>]
    member __.OccursFree5() =
        let x = ()
        Assert.IsTrue(Term.occursFree "x"
            <| Term.ofExpr <@@(fun z -> x)@@>)
