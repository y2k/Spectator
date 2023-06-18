open System

let getRandom = async { return int (Random.Shared.NextDouble() * 10.0) }

let foo (a: int) (b: int) (c: int) (event: int) = printfn "%O-%O-%O [%O]" a b c event

module Applicative =
    let pure_ f = async { return f }

    let apply ff fa =
        async {
            let! f = ff
            let! a = fa
            return f a
        }

let fooA = Applicative.pure_ foo

let fooA2 = Applicative.apply fooA getRandom
let fooA3 = Applicative.apply fooA2 getRandom
let fooA4 = Applicative.apply fooA3 getRandom

async {
    let! f = fooA4
    f 100

    let! f = fooA4
    f 200

    let! f = fooA4
    f 300

    printfn "=== END ==="
}
|> Async.RunSynchronously
