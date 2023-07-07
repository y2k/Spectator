#r "nuget: SodiumFRP.FSharp, 5.0.6"

module Nitrogen =
    open Sodium.Frp

    [<CompilerMessage("Incomplete hole", 130)>]
    let inline FIXME (x: _) = raise (exn $"Incomplete hole: {x}")

    type Effect =
        { name: string
          param: obj
          [<System.Text.Json.Serialization.JsonIgnore>]
          key: obj }

    type 'a EffectFactory = 'a -> Effect
    type 'a Signal = 'a Stream
    type 'a Store = 'a Cell

    module Effect =
        open System.Collections.Generic
        let private globalStore = Dictionary<obj, obj -> unit>()

        let empty: Effect =
            { param = null
              key = null
              name = "empty" }

        let attachHandler (a: 'a EffectFactory) (f: 'a -> unit) =
            globalStore.Add(a, (fun x -> unbox x |> f))

        let create<'a> name : 'a EffectFactory =
            let mutable key: obj = null

            let r =
                fun x ->
                    { param = box x
                      key = key
                      name = name }

            key <- r
            r

        let batch (xs: Effect list) : Effect =
            { param = box xs
              key = null
              name = "batch" }

    module Signal =
        let create<'a> () : 'a Signal = StreamSink.create<'a> ()

        let snapshot (s: 's Store) (f: 's -> 'a -> 'b) (si: 'a Signal) : 'b Signal =
            Stream.snapshot s (fun a b -> f b a) si

        let send arg (signal: _ Stream) =
            (signal :?> _ StreamSink) |> StreamSink.send arg

        let map f s = Stream.map f s

    module Store =
        let create (def: 'a) xs : 'a Store =
            xs
            // |> List.map (fun (e, f) -> e |> Stream.map (fun e s -> f s e))
            |> Stream.mergeAll (failwithf "Merging of events [%O] and [%O] not supported")
            |> Stream.accum def (<|)

open Nitrogen

type DownloadCallback = unit
let createDownloadCallback (_f: byte array -> 'a) (_signal: 'a Signal) : DownloadCallback = ()

let DownloadFx = Effect.create<string * DownloadCallback> "download"
let LogFx = Effect.create<string> "log"

module TelegramApi =
    type TelegramMessage = { user: string; message: string }
    let TelegramMessage = Signal.create<TelegramMessage> ()

module GlobalEvents =
    type GlobalEvent =
        | Event1 of obj
        | Event2 of obj

    let TimerSignal = Signal.create<System.DateTime> ()

    let Bus = Signal.create<GlobalEvent> ()

module Domain =
    type private State = { items: int }

    let private innerUpdateStore = Signal.create<unit> ()

    let private store =
        [ GlobalEvents.Bus |> Signal.map (fun _ a -> { a with items = a.items + 1 })
          innerUpdateStore |> Signal.map (fun _ a -> a) ]
        |> Store.create { items = 0 }

    let onTimerEvent: Effect Signal =
        GlobalEvents.TimerSignal
        |> Signal.snapshot store (fun _state _msg -> Effect.empty)

    let private handleDownloadComplete = Signal.create<string * byte array> ()

    let onNewBotMessageEvent: Effect Signal =
        TelegramApi.TelegramMessage
        |> Signal.snapshot store (fun state msg ->
            let url = $"https://google.com/?query={msg.message}"

            Effect.batch
                [ LogFx $"LOG: {state} message from {msg.user}"
                  DownloadFx(url, createDownloadCallback (fun data -> url, data) handleDownloadComplete) ])

open System.Text.Json
open Sodium.Frp

let () =
    let actual =
        [ Domain.onNewBotMessageEvent; Domain.onTimerEvent ]
        |> Stream.mergeAll (failwithf "Merging of events [%O] and [%O] not supported")
        |> Stream.accum Effect.empty (fun a _ -> a)

    Effect.attachHandler DownloadFx (printfn "LOG : Effect called %O")

    let log () =
        let value = Cell.sample actual
        let opt = JsonSerializerOptions(WriteIndented = true)
        printfn "======================\n%O" (JsonSerializer.Serialize(value, opt))

    TelegramApi.TelegramMessage |> Signal.send { user = "y2k"; message = "hello" }
    log ()

    TelegramApi.TelegramMessage |> Signal.send { user = "admin"; message = "world" }
    log ()
