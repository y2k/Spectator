#r "nuget: SodiumFRP.FSharp, 5.0.6"

module Nitrogen =
    open Sodium.Frp

    [<CompilerMessage("Incomplete hole", 130)>]
    let inline FIXME (x: _) = raise (exn $"Incomplete hole: {x}")

    type Effect =
        { name: string
          param: obj
          [<System.Text.Json.Serialization.JsonIgnore>]
          action: unit -> unit }

    type 'a Signal = 'a Stream
    type 'a SignalProducer = 'a StreamSink
    type 'a Store = 'a Cell

    module Effect =
        let empty: Effect =
            { param = null
              action = ignore
              name = "empty" }

        type 'a EffectFactory =
            private
                { name: string
                  mutable action: 'a -> unit }

        let call (ef: 'a EffectFactory) (param: 'a) : Effect =
            { name = ef.name
              param = box param
              action = fun _ -> ef.action param }

        let attachHandler (ef: 'a EffectFactory) (f: 'a -> unit) = ef.action <- f

        let create<'a> (name: string) : 'a EffectFactory =
            { name = name
              action = fun _ -> failwithf "Effect handle not set for %s" name }

        let batch (xs: Effect list) : Effect =
            { param = box xs
              action = fun _ -> xs |> List.iter (fun e -> e.action ())
              name = "batch" }

    module Signal =
        let create<'a> () : 'a SignalProducer = StreamSink.create<'a> ()

        let snapshot (s: 's Store) (f: 's -> 'a -> 'b) (si: 'a Signal) : 'b Signal =
            Stream.snapshot s (fun a b -> f b a) si

        let send arg (signal: _ SignalProducer) =
            (signal :?> _ StreamSink) |> StreamSink.send arg

        let map f s = Stream.map f s

        let merge xs =
            Stream.mergeAll (failwithf "Merging of signal [%O] and [%O] not supported") xs

    module Store =
        let create (def: 'a) xs : 'a Store =
            xs
            // |> List.map (fun (e, f) -> e |> Stream.map (fun e s -> f s e))
            |> Stream.mergeAll (failwithf "Merging of events [%O] and [%O] not supported")
            |> Stream.accum def (<|)

open Nitrogen

type DownloadCallback = byte array -> unit

let createDownloadCallback (_f: byte array -> 'a) (_signal: 'a SignalProducer) : DownloadCallback =
    fun (data: byte array) -> _signal |> Signal.send (_f data)

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

    let private onTimerEvent: Effect Signal =
        GlobalEvents.TimerSignal
        |> Signal.snapshot store (fun _state _msg -> Effect.empty)

    let DownloadComplete = Signal.create<string * byte array> ()

    let private onHandleDownloadComplete: Effect Signal =
        DownloadComplete
        |> Signal.snapshot store (fun _state _msg -> Effect.call LogFx "Called onHandleDownloadComplete")

    let private onNewBotMessageEvent: Effect Signal =
        TelegramApi.TelegramMessage
        |> Signal.snapshot store (fun state msg ->
            let url = $"https://google.com/?query={msg.message}"

            Effect.batch
                [ Effect.call LogFx $"{state} message from {msg.user}"
                  Effect.call DownloadFx (url, createDownloadCallback (fun data -> url, data) DownloadComplete) ])

    let main =
        [ onHandleDownloadComplete; onNewBotMessageEvent; onTimerEvent ] |> Signal.merge

open System.Text.Json
open Sodium.Frp

let () =
    let actual =
        Domain.main
        |> Stream.accum (ref [ Effect.empty ]) (fun a xs ->
            xs.Value <- a :: xs.Value
            xs)

    Effect.attachHandler DownloadFx (fun (_url, _cb) -> printfn "[DownloadFx] called with = %O" _url)
    // Effect.attachHandler LogFx (printfn "[LogFx] called with = %s")
    Effect.attachHandler DownloadFx ignore

    let log () =
        let value = Cell.sample actual
        let opt = JsonSerializerOptions(WriteIndented = true)
        printfn "======================\n%O" (JsonSerializer.Serialize(value.Value, opt))
        value.Value <- []

    TelegramApi.TelegramMessage |> Signal.send { user = "y2k"; message = "hello" }
    log ()

    Domain.DownloadComplete |> StreamSink.send ("", [||])

    log ()

// TelegramApi.TelegramMessage |> Signal.send { user = "admin"; message = "world" }
// log ()
