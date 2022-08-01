namespace LambdaCalculus

module RosenbergStrong =

    open System

    type nat = int

    let unpair (z: nat): nat * nat = 
        let m = int(Math.Floor(Math.Sqrt(z)))
        let m2 = m * m
        if z - m2 < m then
            (z - m2, m)
        else
            (m, m2  + 2 * m - z)

    let pair (x: nat) (y: nat): nat =
        let m = max x y
        m * m + m + x - y

    let (|Pair|) n = unpair n
    let (|Nat|) (x, y) = pair x y