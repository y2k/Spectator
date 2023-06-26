#r "nuget: SodiumFRP.FSharp, 5.0.6"

open System
open Sodium.Frp

let c1 () =
    let s = CellSink.create 0

    async {
        while true do
            do! Async.Sleep(Random.Shared.Next(1000, 2000))
            CellSink.send (Random.Shared.Next(1000)) s
    }
    |> Async.Start

    s |> Cell.map (sprintf "<button>%i/<button>")

let c2 = Cell.lift2 (sprintf "<div>%s%s</div>") (c1 (), c1 ())

c2 |> Cell.listen (printfn "LOG: %A")

Console.ReadKey()
