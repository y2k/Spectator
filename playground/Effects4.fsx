#r "nuget: SodiumFRP.FSharp, 5.0.6"
#r "nuget: FSharp.SystemTextJson, 1.1.23"

[<CompilerMessage("Incomplete hole", 130)>]
let inline FIXME (x: _) = raise (exn $"Incomplete hole: {x}")

type World = unit

type Effect =
    { name: string
      param: obj
      [<System.Text.Json.Serialization.JsonIgnore>]
      action: World -> unit }

let merge (fs: obj list) : Effect =
    { param = box fs
      name = "Merge"
      action = fun _ -> FIXME "" }

// let dispatch (msg: _) =
//     { param = box msg
//       name = "Dispatch"
//       action = fun _ -> FIXME "" }

type Dispatch = Dispatch of param: obj

module TelegramApi =
    let sendMessage (user: string) (message: string) =
        { param = box (user, message)
          name = "TelegramSendMessage"
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

    let handle (state: State) (user, (message: string)) : obj =
        match message.Split ' ' with
        | [| "/ls" |] ->
            state.subs
            |> Map.tryFind user
            |> Option.defaultValue []
            |> List.fold (fun s x -> $"{s}\n- {x}") "Your subs:"
            |> TelegramApi.sendMessage user
            |> box
        | [| "/add"; url |] ->
            merge
                [ Dispatch(NewSubscriptionCreated(user, url))
                  TelegramApi.sendMessage user "Subscription created" ]
        | _ -> TelegramApi.sendMessage user "[HELP message]"

open Sodium.Frp
open Global
open System.Text.Json
open System.Text.Json.Serialization

let () =
    let telegramMessageProducer = StreamSink.create ()

    let newSubCreatedProducer: Global.NewSubscriptionCreated StreamSink =
        StreamSink.create ()

    let state =
        [ newSubCreatedProducer |> Stream.map Bot.handleState ]
        |> Stream.mergeAll (fun _ _ -> failwith "")
        |> Stream.accum Bot.emtpy (<|)

    let clearLog = StreamSink.create ()

    let effects =
        [ telegramMessageProducer
          |> Stream.snapshot state (fun e s -> Bot.handle s e)
          |> Stream.map (fun x -> [ x ])
          clearLog |> Stream.map (fun _ -> []) ]
        |> Stream.mergeAll (fun _ _ -> failwith "")
        |> Stream.accum [] (fun x _ -> x)

    let mutable cmdLog = []

    let send msg target =
        StreamSink.send msg target
        let effs = Cell.sample effects

        let rec exec (effs: obj list) =
            effs
            |> List.iter (fun e ->
                match e with
                | :? Dispatch as Dispatch msg ->
                    match msg with
                    | :? NewSubscriptionCreated as x ->
                        Transaction.post (fun _ -> StreamSink.send x newSubCreatedProducer)
                    | _ -> FIXME ""
                | :? Effect as e ->
                    match e.name with
                    | "Merge" ->
                        let params: obj list = unbox e.param
                        exec params
                    | _ -> ()
                | _ -> ())

        exec effs

        let options = JsonSerializerOptions(WriteIndented = true)
        JsonFSharpOptions.Default().AddToJsonSerializerOptions(options)
        printfn "================\n%O" (JsonSerializer.Serialize(effs, options))
        cmdLog <- effs :: cmdLog
        StreamSink.send () clearLog

    send ("y2k", "/ls") telegramMessageProducer
    send ("y2k", "/add https://g.com/") telegramMessageProducer
    // send (NewSubscriptionCreated("y2k", "https://g.com/")) newSubCreatedProducer
    send ("y2k", "/ls") telegramMessageProducer

    let options = JsonSerializerOptions(WriteIndented = false)
    JsonFSharpOptions.Default().AddToJsonSerializerOptions(options)
    let actual = JsonSerializer.Serialize(cmdLog, options)

    if
        """[[{"name":"TelegramSendMessage","param":["y2k","Your subs:\n- https://g.com/"]}],[],[{"name":"Merge","param":[{"name":"Dispatch","param":{"Case":"NewSubscriptionCreated","Fields":["y2k","https://g.com/"]}},{"name":"TelegramSendMessage","param":["y2k","Subscription created"]}]}],[{"name":"TelegramSendMessage","param":["y2k","Your subs:"]}]]"""
        <> actual
    then
        failwithf "%s" actual
