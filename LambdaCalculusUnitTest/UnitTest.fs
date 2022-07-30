namespace LambdaCalculusUnitTest

open Microsoft.VisualStudio.TestTools.UnitTesting
open LambdaCalculus
open LambdaCalculus.Parser

[<TestClassAttribute>]
type UnitTest() =

    let subst arg param body =
        Expr.substitute
            (Expr.parse arg)
            param
            (Expr.parse body)
            |> Expr.toString

    let run str =
        str |> Expr.parse |> Expr.eval

    [<TestMethod>]
    member __.ToString1() =
        Assert.AreEqual(
            "λx.λy.x",
            sprintf "%A" <| Expr.ofQuot <@@ (fun x y -> x) @@>)

    [<TestMethod>]
    member __.ToString2() =
        let z = 0
        Assert.AreEqual(
            "λx.λy.z",
            sprintf "%A" <| Expr.ofQuot <@@ (fun x y -> z) @@>)

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
            <| Expr.ofQuot <@@ fun z x -> x @@>)

    [<TestMethod>]
    member __.OccursFree5() =
        let x = ()
        Assert.IsTrue(Expr.occursFree "x"
            <| Expr.ofQuot <@@ fun z -> x @@>)

    [<TestMethod>]
    member __.OccursFree6() =
        let x = ()
        Assert.IsTrue(Expr.occursFree "x"
            <| Expr.ofQuot <@@ (fun x -> x) x @@>)

    [<TestMethod>]
    member __.αConvert1() =
        let before = Expr.ofQuot <@@ fun x y -> x @@>
        let after = Expr.ofQuot <@@ fun z y -> z @@>
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
                |> Expr.betaReduce
                |> Expr.toString
        Assert.AreEqual("((z (v u)) v)", actual)

    [<TestMethod>]
    member __.Eval1() =
        let expr = run "(λx.((x y) (y x)) (λw.(w w) z))"
        Assert.AreEqual("(((z z) y) (y (z z)))", expr.ToString())

    [<TestMethod>]
    member __.Eval2() =
        let expr = run "(λx.m (λx.(x x) λx.(x x)))"
        Assert.AreEqual("m", expr.ToString())

    [<TestMethod>]
    member __.Eval3() =
        let expr =
            sprintf "((%A %A) %A)" And True False
                |> run
        Assert.AreEqual(False, expr)

    [<TestMethod>]
    member __.Eval4() =
        let expr =
            sprintf "((%A %A) %A)" Or True False
                |> run
        Assert.AreEqual(True, expr)

    [<TestMethod>]
    member __.Eval5() =
        let expr =
            sprintf "(((%A %A) %A) %A)" If True True False
                |> run
        Assert.AreEqual(True, expr)

    [<TestMethod>]
    member __.Eval6() =

        let expr =
            sprintf "(%A %A)" Succ Zero |> run
        Assert.AreEqual(One, expr)

        let expr =
            sprintf "(%A %A)" Succ expr |> run
        Assert.AreEqual(Two, expr)

        let expr =
            sprintf "((%A %A) %A)" Plus One Two |> run
        Assert.AreEqual(Three, expr)

        let expr =
            sprintf "((%A %A) %A)" Mult Three Two |> run
        Assert.AreEqual(Six, expr)

        let expr =
            sprintf "((%A %A) ((%A %A) %A))" Mult Three Mult Two One |> run
        Assert.AreEqual(Six, expr)

        let expr =
            sprintf "((%A %A) %A)" Mult Three Three |> run
        Assert.AreEqual(Nine, expr)

        let expr =
            sprintf "((%A %A) %A)" Mult Two Five |> run
        Assert.AreEqual(Ten, expr)

    [<TestMethod>]
    member __.Recursion1() =

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
            sprintf "(%A %A)" TriangleRecursive One |> Expr.parse |> Expr.eval
        Assert.AreEqual(One, expr)

        let expr =
            sprintf "(%A %A)" TriangleRecursive Two |> Expr.parse |> Expr.eval
        Assert.AreEqual(Three, expr)

        let expr =
            sprintf "(%A %A)" TriangleRecursive Three |> Expr.parse |> Expr.eval
        Assert.AreEqual(Six, expr)

        let expr =
            sprintf "(%A %A)" TriangleRecursive Four |> Expr.parse |> Expr.eval
        Assert.AreEqual(Ten, expr)

    /// Factorial is too slow?
    (*
    [<TestMethod>]
    member __.Recursion2() =

        let IsZero =
            sprintf "λn.((n λx.%A) %A)" False True
                |> Expr.parse
        let Pred =
            "λn.λf.λx.(((n λg.λh.(h (g f))) λu.x) λu.u)"
                |> Expr.parse
        let FactorialNonRecursive =
            sprintf "λg.λn.(((%A (%A n)) %A) ((%A n) (g (%A n))))" If IsZero One Mult Pred
                |> Expr.parse
        let FactorialRecursive =
            sprintf "(%A %A)" Y FactorialNonRecursive
                |> Expr.parse

        let expr =
            sprintf "(%A %A)" FactorialRecursive One |> Expr.parse |> Expr.eval
        Assert.AreEqual(One, expr)

        let expr =
            sprintf "(%A %A)" FactorialRecursive Two |> Expr.parse |> Expr.eval
        Assert.AreEqual(Two, expr)
    *)
