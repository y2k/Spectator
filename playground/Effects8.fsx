#r "nuget: SodiumFRP.FSharp, 5.0.6"
#r "nuget: FSharp.SystemTextJson, 1.1.23"
#r "nuget: DiffPlex, 1.7.1"
#r "nuget: BrotliSharpLib, 0.3.3"

open System

[<AutoOpen>]
module Prelude =
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

    let assertEffect (cmdLog: _ list) expectedEnc =
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
        let actual = JsonSerializer.Serialize(cmdLog |> List.map id, options)

        if expected <> actual then
            let diff = Differ.diff expected actual

            let encoded =
                actual
                |> Text.Encoding.UTF8.GetBytes
                |> fun bytes -> BrotliSharpLib.Brotli.CompressBuffer(bytes, 0, bytes.Length)
                |> Convert.ToBase64String

            failwithf "\n------------\n%s\n------------\n%s\n------------" diff encoded

module Effect =
    type Context = private { _TODO_: obj }
    type Effect = Context -> unit

    let unsafeRun f : unit = f { _TODO_ = null }

    let inline createEmptyEffect name a : Effect =
        raise (exn $"Effect '%s{name}' not implemented {a}")

    let empty: Effect = fun _ -> ()

    let batch (fs: Effect list) : Effect =
        fun world -> fs |> List.iter (fun f -> f world)

    let join a b = batch [ a; b ]

open Sodium.Frp

module TelegramApi =
    let mutable sendMessage =
        fun (user: string) (message: string) -> Effect.createEmptyEffect "SendTelegramMessage" (user, message)

    let telegramMessageProducer = StreamSink.create ()

module HttpApi =
    let mutable download =
        fun (url: string) (_callback: byte[] -> obj) -> Effect.createEmptyEffect "Download" url

    let mutable download2 =
        fun (url: string) (_callback: byte[] -> Effect.Effect) -> Effect.createEmptyEffect "Download" url

module Id =
    type t = private Id of string

    let create (xs: string list) : t =
        xs |> List.reduce (sprintf "%s:%s") |> Id

let timerTicked = StreamSink.create<unit> ()

module Global =
    type ProviderId = Rss

    type GlobalEvent =
        | NewSubscriptionCreated of user: string * url: string * id: Id.t
        | NewSubscriptionRemoved of user: string * id: Id.t
        | SubscriptionCreated of user: string * url: string * provider: ProviderId
        | NewSnapshotCreated of user: string * url: string

    let mutable dispatch = Effect.createEmptyEffect "dispatch"
    let globalEventProducer = StreamSink.create ()

(* Logic *)

module Bot =
    open Global

    type private State =
        { newSubs: Map<string, (string * Id.t) list>
          subs: Map<string, (string * ProviderId) list> }

    let private update e (state: State) : State =
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

    let private state =
        [ globalEventProducer |> Stream.map update ]
        |> Stream.mergeAll (failwithf "(Bot) Can't merge: %O %O")
        |> Stream.accum
            { newSubs = Map.empty
              subs = Map.empty }
            (<|)

    let private onNewMessage (user, (message: string)) (state: State) =
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
            Effect.batch
                [ dispatch (NewSubscriptionCreated(user, url, Id.create [ user; url ]))
                  TelegramApi.sendMessage user "Subscription created" ]
        | _ -> TelegramApi.sendMessage user "Unknown command. Enter /start to show help."

    let effects =
        TelegramApi.telegramMessageProducer |> Stream.snapshot state onNewMessage

module SubWorker =
    open Global

    type private State = { newSubs: Id.t Set }

    let private state =
        [ globalEventProducer
          |> Stream.map (fun e s ->
              match e with
              | NewSubscriptionCreated(_, _, id) ->
                  { s with
                      newSubs = s.newSubs |> Set.add id }
              | NewSubscriptionRemoved(_, id) ->
                  { s with
                      newSubs = s.newSubs |> Set.remove id }
              | _ -> s) ]
        |> Stream.mergeAll (failwithf "(Sub) Can't merge: %O %O")
        |> Stream.accum { newSubs = Set.empty } (<|)

    type DownloadCompleted = DownloadCompleted of id: Id.t * user: string * url: string * data: byte array

    let downloadCompleteProducer = StreamSink.create ()

    let effects =
        [ globalEventProducer
          |> Stream.map (function
              | NewSubscriptionCreated(user, url, id) ->
                  HttpApi.download url (fun data -> DownloadCompleted(id, user, url, data))
              | _ -> Effect.empty)

          downloadCompleteProducer
          |> Stream.snapshot state (fun (DownloadCompleted(newSubId, user, url, (data: byte array))) (_: State) ->
              let isRss = (fun _ -> true) data

              let e1 =
                  if isRss then
                      [ dispatch (SubscriptionCreated(user, url, Rss)) ]
                  else
                      []

              Effect.batch (e1 @ [ dispatch (NewSubscriptionRemoved(user, newSubId)) ])) ]
        |> Stream.mergeAll (failwithf "Can't merge: %O %O")

module SnapshotWorker =
    type private State =
        { subs: {| id: Guid; url: string |} list
          pending: Guid Set }

    let private downloadCompleted = StreamSink.create<Guid * string * byte array> ()
    let private downloadStarted = StreamSink.create<Guid> ()

    let private state =
        [ downloadStarted
          |> Stream.map (fun id state ->
              { state with
                  pending = Set.add id state.pending })
          downloadCompleted
          |> Stream.map (fun (id, _, _) state ->
              let (downSubs, otherSubs) = state.subs |> List.partition (fun x -> x.id = id)

              { state with
                  subs = otherSubs @ downSubs
                  pending = Set.remove id state.pending }) ]
        |> Stream.mergeAll (>>)
        |> Stream.accum
            { subs = List.empty
              pending = Set.empty }
            (<|)

    let effects =
        [ timerTicked
          |> Stream.snapshot state (fun () state ->
              state.subs
              |> List.take (3 - Seq.length state.pending)
              |> List.collect (fun x ->
                  [ HttpApi.download2 x.url (fun data _ -> StreamSink.send (x.id, x.url, data) downloadCompleted)
                    fun _ -> StreamSink.send x.id downloadStarted ])
              |> Effect.batch)
          downloadCompleted
          |> Stream.snapshot state (fun (_id, _url, _data) _state ->
              FIXME ""
              FIXME "") ]
        |> Stream.mergeAll Effect.join

module Notifications =
    open Global

    let effects =
        globalEventProducer
        |> Stream.map (function
            | NewSnapshotCreated(user, url) -> TelegramApi.sendMessage user $"New Snapshot: {url}"
            | _ -> Effect.empty)

let () =
    let effects =
        [ Bot.effects; Notifications.effects; SubWorker.effects ]
        |> Stream.mergeAll (fun e1 e2 -> Effect.batch [ e1; e2 ])
        |> Stream.accum Effect.empty (fun newEffs _ -> newEffs)

    (*  *)

    let mutable cmdLog = []

    let send msg target =
        let mutable localLog: obj list = []

        TelegramApi.sendMessage <-
            fun user message _ ->
                localLog <-
                    localLog
                    @ [ {| effect = "SendTelegramMessage"
                           payload = {| user = user; message = message |} |} ]

        HttpApi.download <-
            fun url _ _ ->
                localLog <-
                    localLog
                    @ [ {| effect = "Download"
                           payload = {| url = url |} |} ]

        Global.dispatch <-
            fun e _ ->
                localLog <- localLog @ [ {| effect = "Dispatch"; payload = e |} ]
                StreamSink.send e Global.globalEventProducer
                Effect.unsafeRun (Cell.sample effects)

        StreamSink.send msg target
        Effect.unsafeRun (Cell.sample effects)

        cmdLog <- cmdLog @ [ localLog ]

    (*  *)

    send ("y2k", "/ls") TelegramApi.telegramMessageProducer
    send ("y2k", "/add https://g.com/") TelegramApi.telegramMessageProducer
    send ("y2k", "/ls") TelegramApi.telegramMessageProducer

    send
        (SubWorker.DownloadCompleted(Id.create [ "y2k"; "https://g.com/" ], "y2k", "https://g.com/", [||]))
        SubWorker.downloadCompleteProducer

    send ("y2k", "/ls") TelegramApi.telegramMessageProducer

    send (Global.NewSnapshotCreated("y2k", "https://g.com/1")) Global.globalEventProducer

    (*  *)

    assertEffect
        cmdLog
        """G9IFAIzEOBbxRhPCQVDInMpe3hdlNfE2BwzJoqAzcE7A4XzEG0MVZmDbrOpZoCXgm93U7rMFHxJYuV6vv2WAuoEB38kEgfQ16vWU68x38+VlvOdTlqb6afzGrWhrC3RvbMFJTq/3ydWz00HI0RLr7W61yf/2ru5DZPjrawb1gfFSzp/9o4gxkbB2QFdgXXGy9i2q/DKc9evT73EMSmZOrPPsKgXkv6bv6VY8pFSG5P39stOI+zxn/JjWiBWZ1hGiO0utZXoHo5scXoN9pg9TC5LinOYf1bpeJazx04YYIQzm+KmxUJn8VUse4wPFAhHQPoxPP5/UhHEfTsW45fo="""
