#r "nuget: SodiumFRP.FSharp, 5.0.6"

[<CompilerMessage("Incomplete hole", 130)>]
let inline FIXME (x: _) = raise (exn $"Incomplete hole: {x}")

module Sample6 =
    open Sodium.Frp

    type Effect = { value: obj; key: obj }
    type 'a EffectFactory = 'a -> Effect
    type 'a Signal = 'a Stream

    type 'a Store = 'a Cell

    module Effect =
        open System.Collections.Generic
        let private globalStore = Dictionary<obj, obj -> unit>()

        let empty: Effect = { value = null; key = null }

        let attachHandler (a: 'a EffectFactory) (f: 'a -> unit) =
            globalStore.Add(a, (fun x -> unbox x |> f))

        let create<'a> () : 'a EffectFactory =
            let mutable key: obj = null
            let r = fun x -> { value = box x; key = key }
            key <- r
            r

        let batch (xs: Effect list) : Effect = { value = box xs; key = FIXME "" }

    module Signal =
        let create<'a> () : 'a Signal = StreamSink.create<'a> ()

        let snapshot (s: 's Store) (f: 's -> 'a -> 'b) (si: 'a Signal) : 'b Signal =
            Stream.snapshot s (fun a b -> f b a) si

    module Store =
        let create (def: 'a) xs : 'a Store =
            xs
            |> List.map (fun (e, f) -> e |> Stream.map (fun e s -> f s e))
            |> Stream.mergeAll (fun x _ -> x)
            |> Stream.accum def (<|)

    let DownloadFx = Effect.create<string * (byte array -> Effect)> ()
    let LogFx = Effect.create<string> ()

    module TelegramApi =
        type TelegramMessage = { user: string; message: string }
        let TelegramMessage = Signal.create<TelegramMessage> ()

    module GlobalEvents =
        type GlobalEvent =
            | Event1 of obj
            | Event2 of obj

        let Bus = Signal.create<GlobalEvent> ()

    module Domain =
        type State = { items: obj list }

        let store = [ GlobalEvents.Bus, (fun a _ -> a) ] |> Store.create { items = [] }

        let handleDownload (url: string) (data: byte array) = LogFx $"{url} - {data}"

        let handleDownload_ (url: string) (state: State) (data: byte array) = LogFx $"{url} - {data}"

        let downloadCompleted = Signal.create<byte array> ()

        let handleDownload__ =
            downloadCompleted
            |> Signal.snapshot store (fun state data -> handleDownload_ (FIXME "") state data)

        let onNewBotMessage state (msg: TelegramApi.TelegramMessage) : Effect =
            let url = $"https://google.com/?query={msg.message}"

            Effect.batch
                [ LogFx $"LOG : ${state} message from {msg.user}"
                  DownloadFx(url, handleDownload url) ]

        let onNewBotMessageEvent: Effect Signal =
            TelegramApi.TelegramMessage |> Signal.snapshot store onNewBotMessage

    open System.Text.Json

    let () =
        let actual = Domain.onNewBotMessageEvent |> Stream.accum Effect.empty (fun a _ -> a)

        Effect.attachHandler DownloadFx (printfn "LOG : Effect called %O")

        let log () =
            let value = Cell.sample actual
            printfn "===============\n%O\n---\n%O" (JsonSerializer.Serialize value) value

        log ()

        (TelegramApi.TelegramMessage :?> _ StreamSink)
        |> StreamSink.send { user = "y2k"; message = "hello" }

        log ()

        (TelegramApi.TelegramMessage :?> _ StreamSink)
        |> StreamSink.send { user = "admin"; message = "world" }

        log ()
