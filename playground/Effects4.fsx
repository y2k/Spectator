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

type Dispatch = Dispatch of param: obj

let asEffect (data: obj) : Effect =
    { name = "asEffect"
      param = data
      action = ignore }

module TelegramApi =
    let sendMessage (user: string) (message: string) =
        { param = box (user, message)
          name = "TelegramSendMessage"
          action = fun _ -> FIXME "" }

module HttpApi =
    type DownloadFx = DownloadFx of url: string * callback: (byte[] -> Effect)

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

    type NewSnapshotCreated = NewSnapshotCreated of user: string * url: string

(* Logic *)

module Bot =
    open Global

    type State =
        { newSubs: Map<string, (string * Id.t) list>
          subs: Map<string, (string * ProviderId) list> }

    let empty =
        { newSubs = Map.empty
          subs = Map.empty }

    let private updateLocal state user fsubs fupdate f =
        fsubs state
        |> Map.tryFind user
        |> Option.defaultValue []
        |> fun xs -> f xs
        |> fun xs -> Map.add user xs (fsubs state)
        |> fupdate state

    let update e (state: State) : State =
        match e with
        | SubscriptionCreated(user, url, pId) ->
            updateLocal state user (fun x -> x.subs) (fun s x -> { s with subs = x }) (fun xs -> (url, pId) :: xs)
        | NewSubscriptionCreated(user, url, id) ->
            updateLocal state user (fun x -> x.newSubs) (fun s x -> { s with newSubs = x }) (fun xs -> (url, id) :: xs)
        | NewSubscriptionRemoved(user, newId) ->
            updateLocal
                state
                user
                (fun x -> x.newSubs)
                (fun s x -> { s with newSubs = x })
                (List.filter (fun (_, id) -> id <> newId))

    let handle (user, (message: string)) (state: State) : obj =
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
            |> box
        | [| "/add"; url |] ->
            merge
                [ Dispatch(NewSubscriptionCreated(user, url, Id.create [ user; url ]))
                  TelegramApi.sendMessage user "Subscription created" ]
        | _ -> TelegramApi.sendMessage user "Unknown command. Enter /start to show help."

module SubWorker =
    open Global
    open HttpApi

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
                Dispatch(SubscriptionCreated(user, url, ProviderId.Rss)) |> asEffect
            else
                merge []

        merge [ e1; Dispatch(NewSubscriptionRemoved(user, newSubId)) ]

    let handle e : obj =
        match e with
        | NewSubscriptionCreated(user, url, id) ->
            DownloadFx(url, (fun data -> Dispatch(DownloadCompleted(id, user, url, data)) |> asEffect))
        | _ -> merge []

module Notifications =
    open Global

    let handle (NewSnapshotCreated(user, url)) =
        TelegramApi.sendMessage user $"New Snapshot: {url}"

open Sodium.Frp
open Global

let () =
    let clearLog = StreamSink.create ()
    let telegramMessageProducer = StreamSink.create ()
    let globalEventProducer = StreamSink.create ()
    let subWorkerDownloadCompletedProducer = StreamSink.create ()
    let newSnapCreated = StreamSink.create ()

    let botState =
        [ globalEventProducer |> Stream.map Bot.update ]
        |> Stream.mergeAll (failwithf "%O")
        |> Stream.accum Bot.empty (<|)

    let subState =
        [ globalEventProducer |> Stream.map SubWorker.update ]
        |> Stream.mergeAll (failwithf "%O")
        |> Stream.accum SubWorker.empty (<|)

    let effects =
        [ telegramMessageProducer
          |> Stream.snapshot botState Bot.handle
          |> Stream.map (fun x -> Some x)

          newSnapCreated
          |> Stream.map Notifications.handle
          |> Stream.map (fun x -> Some x)

          globalEventProducer
          |> Stream.snapshot botState (fun e _ -> SubWorker.handle e)
          |> Stream.map (fun x -> Some x)
          subWorkerDownloadCompletedProducer
          |> Stream.snapshot subState SubWorker.downloadCompleted
          |> Stream.map (fun x -> Some x)

          clearLog |> Stream.map (fun _ -> None) ]
        |> Stream.mergeAll (failwithf "%O")
        |> Stream.accum [] (fun e effs ->
            match e with
            | Some e -> effs @ [ e ]
            | None -> [])

    (*  *)

    let mutable cmdLog = []

    let send msg target =
        StreamSink.send msg target

        let rec exec (effs: obj list) =
            effs
            |> List.iter (fun e ->
                match e with
                | :? Dispatch as Dispatch msg ->
                    match msg with
                    | :? GlobalEvent as x -> Transaction.post (fun _ -> StreamSink.send x globalEventProducer)
                    | m -> failwithf "Unknown message %O" m
                | :? Effect as e ->
                    match e.name with
                    | "Merge" ->
                        let effects: obj list = unbox e.param
                        exec effects
                    | "TelegramSendMessage" -> ()
                    | "asEffect" -> exec [ e.param ]
                    | e -> failwithf "Unknown effect %O" e
                | e -> failwithf "Unknown effect %O" e)

        exec (Cell.sample effects)
        cmdLog <- cmdLog @ Cell.sample effects
        StreamSink.send () clearLog

    (*  *)

    send ("y2k", "/ls") telegramMessageProducer
    send ("y2k", "/add https://g.com/") telegramMessageProducer
    send ("y2k", "/ls") telegramMessageProducer

    send
        (SubWorker.DownloadCompleted(Id.create [ "y2k"; "https://g.com/" ], "y2k", "https://g.com/", [||]))
        subWorkerDownloadCompletedProducer

    send ("y2k", "/ls") telegramMessageProducer

    send (NewSnapshotCreated("y2k", "https://g.com/1")) newSnapCreated

    (*  *)

    assertEffect
        cmdLog
        """WwogIHsKICAgICJuYW1lIjogIlRlbGVncmFtU2VuZE1lc3NhZ2UiLAogICAgInBhcmFtIjogWwogICAgICAieTJrIiwKICAgICAgIllvdXIgc3ViczoiCiAgICBdCiAgfSwKICB7CiAgICAibmFtZSI6ICJNZXJnZSIsCiAgICAicGFyYW0iOiBbCiAgICAgIHsKICAgICAgICAiQ2FzZSI6ICJOZXdTdWJzY3JpcHRpb25DcmVhdGVkIiwKICAgICAgICAiRmllbGRzIjogWwogICAgICAgICAgInkyayIsCiAgICAgICAgICAiaHR0cHM6Ly9nLmNvbS8iLAogICAgICAgICAgInkyazpodHRwczovL2cuY29tLyIKICAgICAgICBdCiAgICAgIH0sCiAgICAgIHsKICAgICAgICAibmFtZSI6ICJUZWxlZ3JhbVNlbmRNZXNzYWdlIiwKICAgICAgICAicGFyYW0iOiBbCiAgICAgICAgICAieTJrIiwKICAgICAgICAgICJTdWJzY3JpcHRpb24gY3JlYXRlZCIKICAgICAgICBdCiAgICAgIH0KICAgIF0KICB9LAogIHsKICAgICJDYXNlIjogIkRvd25sb2FkRngiLAogICAgIkZpZWxkcyI6IFsKICAgICAgImh0dHBzOi8vZy5jb20vIiwKICAgICAge30KICAgIF0KICB9LAogIHsKICAgICJuYW1lIjogIlRlbGVncmFtU2VuZE1lc3NhZ2UiLAogICAgInBhcmFtIjogWwogICAgICAieTJrIiwKICAgICAgIllvdXIgc3Viczpcbi1bSW4gUHJvZ3Jlc3NdIGh0dHBzOi8vZy5jb20vIgogICAgXQogIH0sCiAgewogICAgIm5hbWUiOiAiTWVyZ2UiLAogICAgInBhcmFtIjogWwogICAgICB7CiAgICAgICAgIm5hbWUiOiAiYXNFZmZlY3QiLAogICAgICAgICJwYXJhbSI6IHsKICAgICAgICAgICJDYXNlIjogIlN1YnNjcmlwdGlvbkNyZWF0ZWQiLAogICAgICAgICAgIkZpZWxkcyI6IFsKICAgICAgICAgICAgInkyayIsCiAgICAgICAgICAgICJodHRwczovL2cuY29tLyIsCiAgICAgICAgICAgIHsKICAgICAgICAgICAgICAiQ2FzZSI6ICJSc3MiCiAgICAgICAgICAgIH0KICAgICAgICAgIF0KICAgICAgICB9CiAgICAgIH0sCiAgICAgIHsKICAgICAgICAiQ2FzZSI6ICJOZXdTdWJzY3JpcHRpb25SZW1vdmVkIiwKICAgICAgICAiRmllbGRzIjogWwogICAgICAgICAgInkyayIsCiAgICAgICAgICAieTJrOmh0dHBzOi8vZy5jb20vIgogICAgICAgIF0KICAgICAgfQogICAgXQogIH0sCiAgewogICAgIm5hbWUiOiAiTWVyZ2UiLAogICAgInBhcmFtIjogW10KICB9LAogIHsKICAgICJuYW1lIjogIk1lcmdlIiwKICAgICJwYXJhbSI6IFtdCiAgfSwKICB7CiAgICAibmFtZSI6ICJUZWxlZ3JhbVNlbmRNZXNzYWdlIiwKICAgICJwYXJhbSI6IFsKICAgICAgInkyayIsCiAgICAgICJZb3VyIHN1YnM6XG4tW1Jzc10gaHR0cHM6Ly9nLmNvbS8iCiAgICBdCiAgfSwKICB7CiAgICAibmFtZSI6ICJUZWxlZ3JhbVNlbmRNZXNzYWdlIiwKICAgICJwYXJhbSI6IFsKICAgICAgInkyayIsCiAgICAgICJOZXcgU25hcHNob3Q6IGh0dHBzOi8vZy5jb20vMSIKICAgIF0KICB9Cl0="""
