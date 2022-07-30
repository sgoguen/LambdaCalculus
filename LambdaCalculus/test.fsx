#r "nuget: FParsec"
#load "Expr.fs"
#load "Parser.fs"

open LambdaCalculus
open LambdaCalculus.Parser

let True = <@@ fun x y -> x @@> |> Expr.ofQuot
let False = <@@ fun x y -> y @@> |> Expr.ofQuot
let If = <@@ fun b x y -> b x y @@> |> Expr.ofQuot
let And = sprintf "λp.λq.((p q) %A)" False |> Expr.parse
let Or = sprintf "λp.λq.((p %A) q)" True |> Expr.parse

let Zero =  <@@ fun f x -> x @@> |> Expr.ofQuot   // same as False
let One =   <@@ fun f x -> f x @@> |> Expr.ofQuot
let Two =   <@@ fun f x -> f (f x) @@> |> Expr.ofQuot
let Three = <@@ fun f x -> f (f (f x)) @@> |> Expr.ofQuot
let Four =  <@@ fun f x -> f (f (f (f x))) @@> |> Expr.ofQuot
let Five =  <@@ fun f x -> f (f (f (f (f x)))) @@> |> Expr.ofQuot
let Six =   <@@ fun f x -> f (f (f (f (f (f x))))) @@> |> Expr.ofQuot
let Seven = <@@ fun f x -> f (f (f (f (f (f (f x)))))) @@> |> Expr.ofQuot
let Eight = <@@ fun f x -> f (f (f (f (f (f (f (f x))))))) @@> |> Expr.ofQuot
let Nine =  <@@ fun f x -> f (f (f (f (f (f (f (f (f x)))))))) @@> |> Expr.ofQuot
let Ten =   <@@ fun f x -> f (f (f (f (f (f (f (f (f (f x))))))))) @@> |> Expr.ofQuot

let Succ = <@@ fun n f x -> f ((n f) x) @@> |> Expr.ofQuot
let Plus = <@@ fun m n f x -> (n f) ((m f) x) @@> |> Expr.ofQuot
let Mult = <@@ fun m n f -> m (n f) @@> |> Expr.ofQuot

/// Y-combinator for recursion
let Y = "λh.(λx.(h (x x)) λx.(h (x x)))" |> Expr.parse
