namespace LambdaCalculusUnitTest

open Microsoft.VisualStudio.TestTools.UnitTesting
open LambdaCalculus

[<TestClassAttribute>]
type UnitTest() = 

    [<TestMethod>]
    member __.ToString() =
        Assert.AreEqual(
            "λx.λy.x",
            sprintf "%A" <| Term.ofExpr <@@(fun x y -> x)@@>)
