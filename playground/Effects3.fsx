#r "nuget: SodiumFRP.FSharp, 5.0.6"

[<CompilerMessage("Incomplete hole", 130)>]
let inline FIXME (x: _) = raise (exn $"Incomplete hole: {x}")

type World = unit
type Effect = { param: obj; action: World -> unit }

let merge (fs: Effect list) : Effect =
    { param = box fs
      action = fun _ -> FIXME "" }

let dispatch (msg: _) =
    { param = box msg
      action = fun _ -> FIXME "" }

module TelegramApi =
    let sendMessage (user: string) (message: string) =
        { param = box (user, message)
          action = fun _ -> FIXME "" }

module Global =
    type NewSubscriptionCreated = NewSubscriptionCreated of user: string * url: string

module Bot =
    open Global

    type State = { subs: Map<string, string list> }
    let emtpy = { subs = Map.empty }

    let handleState (NewSubscriptionCreated(user, url)) (state: State) : State =
        { state with
            subs =
                state.subs
                |> Map.tryFind user
                |> Option.defaultValue []
                |> fun xs -> url :: xs
                |> fun xs -> Map.add user xs state.subs }

    let handle (state: State) (user, (message: string)) : Effect =
        match message.Split ' ' with
        | [| "/ls" |] ->
            state.subs
            |> Map.tryFind user
            |> Option.defaultValue []
            |> List.fold (fun s x -> $"{s}\n- {x}") "Your subs:"
            |> TelegramApi.sendMessage user
        | [| "/add"; url |] ->
            merge
                [ dispatch (NewSubscriptionCreated(user, url))
                  TelegramApi.sendMessage user "Subscription created" ]
        | _ -> TelegramApi.sendMessage user "[HELP message]"

open Sodium.Frp
open Global
open System.Text.Json

let () =
    let telegramMessageProducer = StreamSink.create ()

    let newSubCreatedProducer: Global.NewSubscriptionCreated StreamSink =
        StreamSink.create ()

    let state =
        [ newSubCreatedProducer |> Stream.map Bot.handleState ]
        |> Stream.mergeAll (fun _ _ -> FIXME "")
        |> Stream.accum Bot.emtpy (<|)

    let clearLog = StreamSink.create ()

    let effects =
        [ telegramMessageProducer
          |> Stream.snapshot state (fun e s -> Bot.handle s e)
          |> Stream.map (fun x -> [ x ])
          clearLog |> Stream.map (fun _ -> []) ]
        |> Stream.mergeAll (fun _ _ -> FIXME "")
        |> Stream.accum [] (fun x _ -> x)

    let log () =
        let opt = JsonSerializerOptions(WriteIndented = true)
        printfn "================\n%O" (JsonSerializer.Serialize(Cell.sample effects, opt))
        StreamSink.send () clearLog

    StreamSink.send ("y2k", "/ls") telegramMessageProducer
    log ()

    StreamSink.send ("y2k", "/add https://g.com/") telegramMessageProducer
    log ()

    StreamSink.send (NewSubscriptionCreated("y2k", "https://g.com/")) newSubCreatedProducer
    StreamSink.send ("y2k", "/ls") telegramMessageProducer
    log ()
