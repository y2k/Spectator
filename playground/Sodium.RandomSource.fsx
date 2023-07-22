#r "nuget: SodiumFRP.FSharp, 5.0.6"

open Sodium.Frp
open System

module Generator =
    type t = private { seed: int }

    let empty = { seed = 0 }
    let create () = { seed = Random.Shared.Next() }
    let createFrom seed = { seed = seed }

    let next (generator: t) : (double * t) =
        let r = Random(generator.seed)
        r.NextDouble(), { seed = r.Next() }

let source = CellSink.create Generator.empty

let a: unit StreamSink = StreamSink.create ()
let b: unit StreamSink = StreamSink.create ()

let a2 =
    a
    |> Stream.snapshot source (fun _ t -> Generator.next t |> fst |> ((*) 1000.0) |> int)

let b2 =
    b
    |> Stream.snapshot source (fun _ t -> Generator.next t |> fst |> ((*) 1000.0) |> int)

a2 |> Stream.listen (printfn "A) %O")
b2 |> Stream.listen (printfn "B) %O")

StreamSink.send () a
StreamSink.send () b
