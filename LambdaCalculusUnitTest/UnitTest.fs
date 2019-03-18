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
       
    /// subst((w z), y, λx.y) -> λx.(w z)
    [<TestMethod>]
    member __.Substitute1() =

        let w () = ()
        let z = ()
        let arg = Expr.ofQuot <@@w z@@>
        let param = "y"
        let body =
            let y = ()
            Expr.ofQuot <@@fun x -> y@@>

        let actual = Expr.substitute arg param body
        let expected = Expr.ofQuot <@@fun x -> w z@@>
        Assert.AreEqual(expected, actual)

    /// subst((w x), y, λx.x) -> λa.a
    [<TestMethod>]
    member __.Substitute2() =

        let arg =
            let w () = ()
            let x = ()
            Expr.ofQuot <@@w x@@>
        let param = "y"
        let body = Expr.ofQuot <@@fun x -> x@@>

        let actual = Expr.substitute arg param body
        let expected = Expr.ofQuot <@@fun a -> a@@>
        Assert.AreEqual(expected, actual)

    /// subst( (λy.y (y x)), y, λy.x )
    [<TestMethod>]
    member __.Substitute3() =

        let arg =
            let x = ()
            let y () = ()
            Expr.ofQuot <@@(fun y -> y) (y x)@@>
        let param = "y"
        let body =
            let x = ()
            Expr.ofQuot <@@fun y -> x@@>

        let actual = Expr.substitute arg param body
        let expected = body
        Assert.AreEqual(expected, actual)

    /// subst( (λz.z x), x, (((z z) λz.x) λx.x) )
    [<TestMethod>]
    member __.Substitute4() =

        let arg =
            let x = ()
            Expr.ofQuot <@@(fun z -> z) x@@>
        let param = "x"
        let body =
            let x = ()
            let z_z = Application (Variable "z", Variable "z")
            Application (
                Application (z_z, Expr.ofQuot <@@fun z -> x@@>),
                Expr.ofQuot <@@fun x -> x@@>)
        Assert.AreEqual("(((z z) λz.x) λx.x)", body.ToString())

        let actual = Expr.substitute arg param body
        Assert.AreEqual("(((z z) λz.(λz.z x)) λx.x)", actual.ToString())

    /// subst( ((z z) x), y, λy.λx.(y x) )
    [<TestMethod>]
    member __.Substitute5() =

        let arg =
            Application (
                Application (Variable "z", Variable "z"),
                Variable "x")
        let param = "y"
        let body = Expr.ofQuot <@@fun y x -> y x@@>

        let actual = Expr.substitute arg param body
        Assert.AreEqual(body, actual)
