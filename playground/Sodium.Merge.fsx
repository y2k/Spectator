#r "nuget: SodiumFRP.FSharp, 5.0.6"

open Sodium.Frp

// let state = StreamSink.create<int> ()

// let a = Stream.map (fun x -> x) state
// let b = Stream.map (fun x -> 1_000 * x) state

// let out =
//     // Stream.mergeAll (fun a b -> a + b) [ a; b ]
//     Stream.orElseAll [ a; b ] |> Stream.accum [] (fun x xs -> x :: xs)

// StreamSink.send 1 state
// printfn "OUT: %O" (Cell.sample out)

// StreamSink.send 2 state
// printfn "OUT: %O" (Cell.sample out)


let a = StreamSink.create<int> ()
let b = StreamSink.create<int> ()

let out =
    // Stream.mergeAll (fun a b -> a + b) [ a; b ]
    Stream.orElseAll [ a; b ] |> Stream.accum [] (fun x xs -> x :: xs)

StreamSink.send 1 a
StreamSink.send 2 b
StreamSink.send 1000 b
StreamSink.send 2000 b
StreamSink.send 3 b
StreamSink.send 3000 b

printfn "OUT: %A" (Cell.sample out |> List.rev)
