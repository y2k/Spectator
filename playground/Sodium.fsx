#r "nuget: SodiumFRP.FSharp, 5.0.6"

open System
open Sodium.Frp

let stream1 = StreamSink.create<int> ()
let state = Stream.accum 0 ((+)) stream1

Transaction.post (fun _ ->
    StreamSink.send 10 stream1
    printfn "State = %O" (Cell.sample state))

// StreamSink.send 100 stream1
// printfn "State = %O" (Cell.sample state)
