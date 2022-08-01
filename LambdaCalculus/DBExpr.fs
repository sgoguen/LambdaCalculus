namespace LambdaCalculus

//  Define De Bruijn Form where variables are represented by indices.
type DBVariable = int (*index*)

type DBExpr =
    | DBVariable of DBVariable
    | DBApplication of (DBExpr (*function*)  * DBExpr (*argument*) )
    | DBLambda of (DBExpr (*body*) )

    member this.MaxVar =
        this
        |> function
            | DBVariable index -> index
            | DBApplication (func, arg) -> max func.MaxVar arg.MaxVar
            | DBLambda (body) -> body.MaxVar

    member this.MaxParameter =
        this
        |> function
            | DBVariable _ -> 0
            | DBApplication (func, arg) -> max func.MaxParameter arg.MaxParameter
            | DBLambda (body) -> body.MaxParameter + 1

    member this.Width =
        this
        |> function
            | DBVariable _ -> 1
            | DBApplication (func, arg) -> func.Width + arg.Width
            | DBLambda (body) -> body.Width

    member this.IsValidFunction = this.MaxVar <= this.MaxParameter

    member this.ToString =
        match this with
        | DBVariable index -> string (index)
        | DBApplication (func, arg) -> sprintf "(%s %s)" (func.ToString) (arg.ToString)
        | DBLambda body -> sprintf "[%s]" (body.ToString)

module DExpr =

    open RosenbergStrong

    let rec maxVar =
        function
        | DBVariable index -> 0, index
        | DBApplication (MaxVar (fp, fv), MaxVar (ap, av)) -> max fp ap, max fv av
        | DBLambda (MaxVar (p, v)) -> (p + 1, v)

    and (|MaxVar|) = maxVar

    let rec toString =
        function
        | DBVariable index -> string (index)
        | DBApplication (func, arg) -> sprintf "(%s %s)" (toString func) (toString arg)
        | DBLambda body -> sprintf "[%s]" (toString body)

    /// fromInt should create a unique DExpr that are valid.
    // let rec fromInt l n =
    //     if n <= l then
    //         DBVariable n
    //     else
    //         let n = n - (l + 1)

    //         if l = 0 then
    //             let (Pair(p, v)) = n
    //             DBApplication(fromInt l p, fromInt l v)
    //         else
    //             let l = l - 1
    //             let t = n % 2
    //             let n = n / 2

    //             if t = 0 then
    //                 let (p, v) = RosenbergStrong.unpair n
    //                 DBApplication((fromInt l p), (fromInt l v))
    //             else
    //                 DBLambda(fromInt l n)

    let intToLambda n =
        let rec intToLambdaRec l n =
            if n <= l then
                DBVariable n
            else
                let n = n - (l + 1)
                let t = n % 2
                let n = n / 2

                if t = 0 then
                    let (Pair (p, v)) = n
                    DBApplication((intToLambdaRec l p), (intToLambdaRec l v))
                else
                    DBLambda(intToLambdaRec (l + 1) n)

        DBLambda(intToLambdaRec 0 n)

    let lambdaToInt expr =
        let rec lambdaToIntRec l (expr: DBExpr) =
            if l = 0 then
                match expr with
                | DBVariable index -> index
                | DBApplication (func, arg) -> 
                    let func = lambdaToIntRec l func
                    let arg = lambdaToIntRec l arg
                    (RosenbergStrong.pair func arg) + 1
                | DBLambda body -> raise(exn("We shouldn't have a lambda here"))
            else
                match expr with
                | DBVariable index -> index
                | DBApplication (func, arg) -> 
                    let func = lambdaToIntRec l func
                    let arg = lambdaToIntRec l arg
                    2 * (RosenbergStrong.pair func arg) + 1
                | DBLambda (body) -> 
                    2 * (lambdaToIntRec (l - 1) body) + 2

        expr
        |> function
            | DBVariable index -> raise (exn "Not a lambda")
            | DBApplication (func, arg) -> raise (exn "Not a lambda")
            | DBLambda body ->
                let l = expr.MaxParameter
                lambdaToIntRec l body


    let intToChar (n: int) = char(n + int ('a')).ToString()
