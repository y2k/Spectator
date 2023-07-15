#r "nuget: SodiumFRP.FSharp, 5.0.6"
#r "nuget: FSharp.SystemTextJson, 1.1.23"
#r "nuget: DiffPlex, 1.7.1"

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

    let assertEffect cmdLog expectedEnc =
        let expected =
            expectedEnc |> Convert.FromBase64String |> Text.Encoding.UTF8.GetString

        let options = JsonSerializerOptions(WriteIndented = true)
        JsonFSharpOptions.Default().AddToJsonSerializerOptions(options)
        let actual = JsonSerializer.Serialize(cmdLog, options)

        if expected <> actual then
            let diff = Differ.diff expected actual
            let encoded = actual |> Text.Encoding.UTF8.GetBytes |> Convert.ToBase64String
            failwithf "\n------------\n%s\n------------\n%s\n------------" diff encoded

open Prelude

type Effect =
    { name: string
      param: obj
      [<System.Text.Json.Serialization.JsonIgnore>]
      action: unit -> unit }

let emptyFx =
    { param = ()
      name = "Empty"
      action = ignore }

let merge (fs: Effect list) : Effect =
    { param = fs
      name = "Batch"
      action = ignore }

let dispatch (e: obj) : Effect =
    { name = "Dispatch"
      param = e
      action = ignore }

module TelegramApi =
    let sendMessage (user: string) (message: string) =
        { param = user, message
          name = "TelegramSendMessage"
          action = ignore }

module HttpApi =
    let download (url: string) (callback: (byte[] -> Effect)) =
        { param = url
          name = "Download"
          action = ignore }

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

    let handle (user, (message: string)) (state: State) : Effect =
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
            merge
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

    let downloadCompleted (DownloadCompleted(newSubId, user, url, (data: byte array))) (_: State) : Effect =
        let isRss: bool = true

        let e1 =
            if isRss then
                [ dispatch (SubscriptionCreated(user, url, ProviderId.Rss)) ]
            else
                []

        merge (e1 @ [ dispatch (NewSubscriptionRemoved(user, newSubId)) ])

    let handle e : Effect =
        match e with
        | NewSubscriptionCreated(user, url, id) ->
            HttpApi.download url (fun data -> dispatch (DownloadCompleted(id, user, url, data)))
        | _ -> emptyFx

module Notifications =
    open Global

    let handle e : Effect =
        match e with
        | NewSnapshotCreated(user, url) -> TelegramApi.sendMessage user $"New Snapshot: {url}"
        | _ -> emptyFx

open Sodium.Frp
open Global

let () =
    let clearLog = StreamSink.create ()
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
        [ telegramMessageProducer
          |> Stream.snapshot botState Bot.handle
          |> Stream.map (fun x -> Some [ x ])

          globalEventProducer
          |> Stream.map Notifications.handle
          |> Stream.map (fun x -> Some [ x ])

          globalEventProducer
          |> Stream.snapshot botState (fun e _ -> SubWorker.handle e)
          |> Stream.map (fun x -> Some [ x ])
          subWorkerDownloadCompletedProducer
          |> Stream.snapshot subState SubWorker.downloadCompleted
          |> Stream.map (fun x -> Some [ x ])

          clearLog |> Stream.map (fun _ -> None) ]
        |> Stream.mergeAll (Option.map2 (@))
        |> Stream.accum [] (fun newEffs _ ->
            match newEffs with
            | Some e -> e
            | None -> [])

    (*  *)

    let mutable cmdLog = []

    let send msg target =
        let mutable log = []
        StreamSink.send msg target

        let rec exec (effs: Effect list) =
            effs
            |> List.collect (fun e ->
                match e.name with
                | "Batch" -> let effects: Effect list = unbox e.param in effects
                | _ -> [ e ])
            |> List.iter (fun e ->
                match e.name with
                | "Dispatch" ->
                    match e.param with
                    | :? GlobalEvent as x ->
                        Transaction.post (fun _ -> StreamSink.send x globalEventProducer)
                        let fs = Cell.sample effects
                        log <- log @ fs
                        exec fs
                    | m -> failwithf "Unknown message %O" m
                | "TelegramSendMessage" -> ()
                | "Download" -> ()
                | "Empty" -> ()
                | e -> failwithf "Unknown effect %O" e)

        let fs = Cell.sample effects
        log <- log @ fs
        exec fs
        cmdLog <- cmdLog @ (log |> List.filter (fun e -> e.name <> "Empty"))

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
        """WwogIHsKICAgICJuYW1lIjogIlRlbGVncmFtU2VuZE1lc3NhZ2UiLAogICAgInBhcmFtIjogWwogICAgICAieTJrIiwKICAgICAgIllvdXIgc3ViczoiCiAgICBdCiAgfSwKICB7CiAgICAibmFtZSI6ICJCYXRjaCIsCiAgICAicGFyYW0iOiBbCiAgICAgIHsKICAgICAgICAibmFtZSI6ICJEaXNwYXRjaCIsCiAgICAgICAgInBhcmFtIjogewogICAgICAgICAgIkNhc2UiOiAiTmV3U3Vic2NyaXB0aW9uQ3JlYXRlZCIsCiAgICAgICAgICAiRmllbGRzIjogWwogICAgICAgICAgICAieTJrIiwKICAgICAgICAgICAgImh0dHBzOi8vZy5jb20vIiwKICAgICAgICAgICAgInkyazpodHRwczovL2cuY29tLyIKICAgICAgICAgIF0KICAgICAgICB9CiAgICAgIH0sCiAgICAgIHsKICAgICAgICAibmFtZSI6ICJUZWxlZ3JhbVNlbmRNZXNzYWdlIiwKICAgICAgICAicGFyYW0iOiBbCiAgICAgICAgICAieTJrIiwKICAgICAgICAgICJTdWJzY3JpcHRpb24gY3JlYXRlZCIKICAgICAgICBdCiAgICAgIH0KICAgIF0KICB9LAogIHsKICAgICJuYW1lIjogIkRvd25sb2FkIiwKICAgICJwYXJhbSI6ICJodHRwczovL2cuY29tLyIKICB9LAogIHsKICAgICJuYW1lIjogIlRlbGVncmFtU2VuZE1lc3NhZ2UiLAogICAgInBhcmFtIjogWwogICAgICAieTJrIiwKICAgICAgIllvdXIgc3Viczpcbi1bSW4gUHJvZ3Jlc3NdIGh0dHBzOi8vZy5jb20vIgogICAgXQogIH0sCiAgewogICAgIm5hbWUiOiAiQmF0Y2giLAogICAgInBhcmFtIjogWwogICAgICB7CiAgICAgICAgIm5hbWUiOiAiRGlzcGF0Y2giLAogICAgICAgICJwYXJhbSI6IHsKICAgICAgICAgICJDYXNlIjogIlN1YnNjcmlwdGlvbkNyZWF0ZWQiLAogICAgICAgICAgIkZpZWxkcyI6IFsKICAgICAgICAgICAgInkyayIsCiAgICAgICAgICAgICJodHRwczovL2cuY29tLyIsCiAgICAgICAgICAgIHsKICAgICAgICAgICAgICAiQ2FzZSI6ICJSc3MiCiAgICAgICAgICAgIH0KICAgICAgICAgIF0KICAgICAgICB9CiAgICAgIH0sCiAgICAgIHsKICAgICAgICAibmFtZSI6ICJEaXNwYXRjaCIsCiAgICAgICAgInBhcmFtIjogewogICAgICAgICAgIkNhc2UiOiAiTmV3U3Vic2NyaXB0aW9uUmVtb3ZlZCIsCiAgICAgICAgICAiRmllbGRzIjogWwogICAgICAgICAgICAieTJrIiwKICAgICAgICAgICAgInkyazpodHRwczovL2cuY29tLyIKICAgICAgICAgIF0KICAgICAgICB9CiAgICAgIH0KICAgIF0KICB9LAogIHsKICAgICJuYW1lIjogIlRlbGVncmFtU2VuZE1lc3NhZ2UiLAogICAgInBhcmFtIjogWwogICAgICAieTJrIiwKICAgICAgIllvdXIgc3Viczpcbi1bUnNzXSBodHRwczovL2cuY29tLyIKICAgIF0KICB9LAogIHsKICAgICJuYW1lIjogIlRlbGVncmFtU2VuZE1lc3NhZ2UiLAogICAgInBhcmFtIjogWwogICAgICAieTJrIiwKICAgICAgIk5ldyBTbmFwc2hvdDogaHR0cHM6Ly9nLmNvbS8xIgogICAgXQogIH0KXQ=="""
