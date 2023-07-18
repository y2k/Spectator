#r "nuget: SodiumFRP.FSharp, 5.0.6"

open Sodium.Frp

module Effect =
    type World = private { world: obj }
    type Effect = World -> unit
    let unsafeRun f = f { world = null }
    let createEmptyEffect a : Effect = failwithf "Effect not implemented %O" a

let mutable PrintFx = Effect.createEmptyEffect

let foo a b =
    let value = string (a + b)
    PrintFx(string (a * b))

PrintFx <- fun a _ -> printfn "LOG:FX %s" a

foo 3 7 |> Effect.unsafeRun
