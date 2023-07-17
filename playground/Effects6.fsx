#r "nuget: SodiumFRP.FSharp, 5.0.6"
#r "nuget: FSharp.SystemTextJson, 1.1.23"
#r "nuget: DiffPlex, 1.7.1"
#r "nuget: BrotliSharpLib, 0.3.3"

module Prelude =
    open System
    open System.Text.Json
    open System.Text.Json.Serialization

    [<CompilerMessage("Incomplete hole", 130)>]
    let inline FIXME (x: _) = raise (exn $"Incomplete hole: {x}")

    module private Differ =
        open DiffPlex.DiffBuilder
        open DiffPlex.DiffBuilder.Model

        let diff originalText modifiedText =
            let diffResult =
                InlineDiffBuilder.Instance.BuildDiffModel(originalText, modifiedText)

            let printLine (line: DiffPiece) =
                match line.Type with
                | ChangeType.Inserted -> sprintf "+ %s" line.Text
                | ChangeType.Deleted -> sprintf "- %s" line.Text
                | ChangeType.Unchanged -> sprintf "  %s" line.Text
                | x -> failwithf "%O" x

            diffResult.Lines |> Seq.map printLine |> Seq.reduce (sprintf "%O\n%O")

    let assertEffect (cmdLog: _ list list) expectedEnc =
        let expected =
            expectedEnc
            |> Convert.FromBase64String
            |> fun bytes ->
                if bytes.Length = 0 then
                    bytes
                else
                    BrotliSharpLib.Brotli.DecompressBuffer(bytes, 0, bytes.Length)
            |> Text.Encoding.UTF8.GetString

        let options = JsonSerializerOptions(WriteIndented = true)
        JsonFSharpOptions.FSharpLuLike().AddToJsonSerializerOptions(options)
        let actual = JsonSerializer.Serialize(cmdLog |> List.map (List.map box), options)

        if expected <> actual then
            let diff = Differ.diff expected actual

            let encoded =
                actual
                |> Text.Encoding.UTF8.GetBytes
                |> fun bytes -> BrotliSharpLib.Brotli.CompressBuffer(bytes, 0, bytes.Length)
                |> Convert.ToBase64String

            failwithf "\n------------\n%s\n------------\n%s\n------------" diff encoded

open Prelude

module Effects =
    type Effect =
        interface
        end

    type Effects = Effect list

    let emptyFx: Effects = []

    let batch (fs: Effects list) : Effects = List.concat fs

open Effects

type DispatchEffect =
    | DispatchEffect of event: obj

    interface Effect

let dispatch (e: obj) : Effects = [ DispatchEffect e ]

module TelegramApi =
    type TelegramMesage =
        | TelegramMesage of user: string * message: string

        interface Effect

    let sendMessage (user: string) (message: string) : Effects = [ TelegramMesage(user, message) ]

module HttpApi =
    type DownloadEffect =
        | DownloadEffect of url: string * callback: (byte[] -> Effects)

        interface Effect

    let download (url: string) (callback: byte[] -> Effects) : Effects = [ DownloadEffect(url, callback) ]

module Id =
    type t = private Id of string

    let create (xs: string list) : t =
        xs |> List.reduce (sprintf "%s:%s") |> Id

module Global =
    type ProviderId = Rss

    type GlobalEvent =
        | NewSubscriptionCreated of user: string * url: string * id: Id.t
        | NewSubscriptionRemoved of user: string * id: Id.t
        | SubscriptionCreated of user: string * url: string * provider: ProviderId
        | NewSnapshotCreated of user: string * url: string

(* Logic *)

module Bot =
    open Global

    type State =
        { newSubs: Map<string, (string * Id.t) list>
          subs: Map<string, (string * ProviderId) list> }

    let empty =
        { newSubs = Map.empty
          subs = Map.empty }

    let update e (state: State) : State =
        let updateLocal user fsubs fupdate f =
            fsubs state
            |> Map.tryFind user
            |> Option.defaultValue []
            |> fun xs -> f xs
            |> fun xs -> Map.add user xs (fsubs state)
            |> fupdate state

        match e with
        | SubscriptionCreated(user, url, pId) ->
            updateLocal user (fun x -> x.subs) (fun s x -> { s with subs = x }) (fun xs -> (url, pId) :: xs)
        | NewSubscriptionCreated(user, url, id) ->
            updateLocal user (fun x -> x.newSubs) (fun s x -> { s with newSubs = x }) (fun xs -> (url, id) :: xs)
        | NewSubscriptionRemoved(user, newId) ->
            updateLocal
                user
                (fun x -> x.newSubs)
                (fun s x -> { s with newSubs = x })
                (List.filter (fun (_, id) -> id <> newId))
        | _ -> state

    let handle (user, (message: string)) (state: State) : Effects =
        match message.Split ' ' with
        | [| "/ls" |] ->
            [ state.subs
              |> Map.tryFind user
              |> Option.defaultValue []
              |> List.map (fun (url, provider) -> $"-[{provider}] {url}")
              state.newSubs
              |> Map.tryFind user
              |> Option.defaultValue []
              |> List.map (fun (url, _) -> $"-[In Progress] {url}") ]
            |> List.concat
            |> List.fold (fun s x -> $"{s}\n{x}") "Your subs:"
            |> TelegramApi.sendMessage user
        | [| "/add"; url |] ->
            batch
                [ dispatch (NewSubscriptionCreated(user, url, Id.create [ user; url ]))
                  TelegramApi.sendMessage user "Subscription created" ]
        | _ -> TelegramApi.sendMessage user "Unknown command. Enter /start to show help."

module SubWorker =
    open Global

    type State = private { newSubs: Id.t Set }
    let empty = { newSubs = Set.empty }

    let update e (s: State) =
        match e with
        | NewSubscriptionCreated(_, _, id) ->
            { s with
                newSubs = s.newSubs |> Set.add id }
        | NewSubscriptionRemoved(_, id) ->
            { s with
                newSubs = s.newSubs |> Set.remove id }
        | _ -> s

    type DownloadCompleted = DownloadCompleted of id: Id.t * user: string * url: string * data: byte array

    let downloadCompleted (DownloadCompleted(newSubId, user, url, (data: byte array))) (_: State) : Effects =
        let isRss: bool = true

        let e1 =
            if isRss then
                [ dispatch (SubscriptionCreated(user, url, ProviderId.Rss)) ]
            else
                []

        batch (e1 @ [ dispatch (NewSubscriptionRemoved(user, newSubId)) ])

    let handle e : Effects =
        match e with
        | NewSubscriptionCreated(user, url, id) ->
            HttpApi.download url (fun data -> dispatch (DownloadCompleted(id, user, url, data)))
        | _ -> emptyFx

module Notifications =
    open Global

    let handle e : Effects =
        match e with
        | NewSnapshotCreated(user, url) -> TelegramApi.sendMessage user $"New Snapshot: {url}"
        | _ -> emptyFx

open Sodium.Frp
open Global

let () =
    let telegramMessageProducer = StreamSink.create ()
    let globalEventProducer = StreamSink.create ()
    let subWorkerDownloadCompletedProducer = StreamSink.create ()

    let botState =
        [ globalEventProducer |> Stream.map Bot.update ]
        |> Stream.mergeAll (failwithf "(Bot) Can't merge: %O %O")
        |> Stream.accum Bot.empty (<|)

    let subState =
        [ globalEventProducer |> Stream.map SubWorker.update ]
        |> Stream.mergeAll (failwithf "(Sub) Can't merge: %O %O")
        |> Stream.accum SubWorker.empty (<|)

    let effects =
        [ telegramMessageProducer |> Stream.snapshot botState Bot.handle
          globalEventProducer |> Stream.map Notifications.handle
          globalEventProducer |> Stream.snapshot botState (fun e _ -> SubWorker.handle e)
          subWorkerDownloadCompletedProducer
          |> Stream.snapshot subState SubWorker.downloadCompleted ]
        |> Stream.mergeAll (@)
        |> Stream.accum [] (fun newEffs _ -> newEffs)

    (*  *)

    let mutable cmdLog = []

    let send msg target =
        let mutable log = []
        StreamSink.send msg target

        let rec exec (effs: Effect list) =
            effs
            |> List.iter (fun e ->
                match e with
                | :? DispatchEffect as (DispatchEffect e) ->
                    match e with
                    | :? GlobalEvent as x ->
                        Transaction.post (fun _ -> StreamSink.send x globalEventProducer)
                        let fs = Cell.sample effects
                        log <- log @ fs
                        exec fs
                    | m -> failwithf "Unknown message %O" m
                | :? TelegramApi.TelegramMesage -> ()
                | :? HttpApi.DownloadEffect -> ()
                | e -> failwithf "Unknown effect %O" e)

        let fs = Cell.sample effects
        log <- log @ fs
        exec fs
        cmdLog <- cmdLog @ [ log ]

    (*  *)

    send ("y2k", "/ls") telegramMessageProducer
    send ("y2k", "/add https://g.com/") telegramMessageProducer
    send ("y2k", "/ls") telegramMessageProducer

    send
        (SubWorker.DownloadCompleted(Id.create [ "y2k"; "https://g.com/" ], "y2k", "https://g.com/", [||]))
        subWorkerDownloadCompletedProducer

    send ("y2k", "/ls") telegramMessageProducer

    send (NewSnapshotCreated("y2k", "https://g.com/1")) globalEventProducer

    (*  *)

    assertEffect
        cmdLog
        """G4QEACwOzPN9QpNmqdeFOO7km3IrZg8ZcqRf6hG1VueUXWrkNODPEohbgUWnb7XI/P3N+/bBok26lcfmKAs7z8eY4IFZIBE+m9dma/rHFF1+Fjj4FQMkrKe6PfLZqs+8rWEAyH+EiubI75x3cWIAFBxNfUD6cxPoeZCQ4oddv+eajFxF1hSy64O8IdVrYUj21yeHRCpUlbThcQTkWVTrNc9PiYX5qCSDu2vADDjLU1ixJW/vKFQPcCxakMv90iuxmVSMPyR+F/88Ac1qwlLPnMULLfQAItnXR7Biq5bnIhQs+faLojSDrZEquQU="""
