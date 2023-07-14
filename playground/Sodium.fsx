#r "nuget: SodiumFRP.FSharp, 5.0.6"

open System
open Sodium.Frp

let () =
    let s1 = StreamSink.create<int> ()
    let s2 = StreamSink.create<int> ()

    let s = Stream.mergeAll (fun _ _ -> failwith "???") [ s1; s2 ]

    s |> Stream.listen (printfn "%O") |> ignore

    StreamSink.send 1 s1
    StreamSink.send 10 s2

let () =
    let s0 = StreamSink.create<int> ()

    let s =
        [ s0 |> Stream.map (fun x -> 10 * x); s0 |> Stream.map (fun x -> 100 * x) ]
        |> Stream.mergeAll (failwithf "??? [%O + %O]")

    s |> Stream.listen (printfn "%O") |> ignore

    StreamSink.send 3 s0
