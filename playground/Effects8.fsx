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

module Generator =
    type t = private { seed: int }

    let empty = { seed = 0 }

    let next (generator: t) : double * t =
        let r = Random(generator.seed)
        r.NextDouble(), { seed = r.Next() }

    let nextGuid { seed = seed } : Guid * t =
        let random = Random(seed)
        let a = Array.init 4 (fun _ -> random.Next())
        let b = Array.zeroCreate 16
        Buffer.BlockCopy(a, 0, b, 0, b.Length)
        Guid b, { seed = random.Next() }

open Sodium.Frp

module StreamSinkFx =
    let mutable sendEventImpl =
        fun (_: unit -> unit) -> Effect.createEmptyEffect "SendEvent" ()

    let send (e: 'a) (target: 'a StreamSink) =
        sendEventImpl (fun _ -> StreamSink.send e target)

module CommonFx =
    let timerTicked = StreamSink.create<unit> ()

module TelegramApi =
    let mutable sendMessage =
        fun (user: string) (message: string) -> Effect.createEmptyEffect "SendTelegramMessage" (user, message)

    let telegramMessageProducer = StreamSink.create ()

module HttpApi =
    let mutable download =
        fun (url: string) (_callback: byte[] -> Effect.Effect) -> Effect.createEmptyEffect "Download2" url

module Global =
    type ProviderId = Rss

    type GlobalEvent =
        | NewSubscriptionCreated of user: string * url: string * id: Guid
        | NewSubscriptionRemoved of user: string * id: Guid
        | SubscriptionCreated of id: Guid * user: string * url: string * provider: ProviderId
        | NewSnapshotCreated of id: Guid * user: string * url: string

    let mutable dispatch = Effect.createEmptyEffect "dispatch"
    let globalEventProducer = StreamSink.create ()

module RandomApi =
    let randomProducer = CellSink.create Generator.empty

(* Logic *)

module Bot =
    open Global

    type private State =
        { newSubs: Map<string, (string * Guid) list>
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
        | SubscriptionCreated(_, user, url, pId) ->
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

    let private onNewMessage (user, (message: string)) r (state: State) =
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
                [ dispatch (NewSubscriptionCreated(user, url, Generator.nextGuid r |> fst))
                  TelegramApi.sendMessage user "Subscription created" ]
        | _ -> TelegramApi.sendMessage user "Unknown command. Enter /start to show help."

    let effects =
        TelegramApi.telegramMessageProducer
        |> Stream.snapshot2 RandomApi.randomProducer state onNewMessage

module SubWorker =
    open Global

    type private State = { newSubs: Guid Set }

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

    type DownloadCompleted = DownloadCompleted of id: Guid * user: string * url: string * data: byte array

    let downloadCompleteProducer = StreamSink.create ()

    let effects =
        [ globalEventProducer
          |> Stream.map (function
              | NewSubscriptionCreated(user, url, id) ->
                  HttpApi.download url (fun data ->
                      StreamSinkFx.send (DownloadCompleted(id, user, url, data)) downloadCompleteProducer)
              | _ -> Effect.empty)

          downloadCompleteProducer
          |> Stream.snapshot2
              RandomApi.randomProducer
              state
              (fun (DownloadCompleted(newSubId, user, url, (data: byte array))) r (_: State) ->
                  let isRss = (fun _ -> true) data

                  let e1 =
                      if isRss then
                          dispatch (SubscriptionCreated(Generator.nextGuid r |> fst, user, url, Rss))
                      else
                          Effect.empty

                  Effect.join e1 (dispatch (NewSubscriptionRemoved(user, newSubId)))) ]
        |> Stream.mergeAll (failwithf "Can't merge: %O %O")

module SnapshotWorker =
    type private State =
        { subs:
            {| user: string
               id: Guid
               url: string |} list
          pending: Guid Set }

    let downloadCompleted = StreamSink.create<Guid * byte array> ()
    let private downloadStarted = StreamSink.create<Guid> ()

    let private state =
        [ Global.globalEventProducer
          |> Stream.map (fun e state ->
              match e with
              | Global.SubscriptionCreated(id, user, url, _) ->
                  { state with
                      subs = {| user = user; id = id; url = url |} :: state.subs }
              | _ -> state)
          downloadStarted
          |> Stream.map (fun id state ->
              { state with
                  pending = Set.add id state.pending })
          downloadCompleted
          |> Stream.map (fun (id, _) state ->
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
        [ CommonFx.timerTicked
          |> Stream.snapshot state (fun () state ->
              state.subs
              |> List.take (min (List.length state.subs) (3 - Seq.length state.pending))
              |> List.collect (fun x ->
                  [ HttpApi.download x.url (fun data -> StreamSinkFx.send (x.id, data) downloadCompleted)
                    StreamSinkFx.send x.id downloadStarted ])
              |> Effect.batch)
          downloadCompleted
          |> Stream.snapshot2 RandomApi.randomProducer state (fun (subId, _data) r state ->
              state.subs
              |> List.tryFind (fun x -> x.id = subId)
              |> Option.map (fun sub ->
                  Global.dispatch (Global.NewSnapshotCreated(Generator.nextGuid r |> fst, sub.user, sub.url)))
              |> Option.defaultValue Effect.empty) ]
        |> Stream.mergeAll Effect.join

module Notifications =
    open Global

    let effects =
        globalEventProducer
        |> Stream.map (function
            | NewSnapshotCreated(_, user, url) -> TelegramApi.sendMessage user $"New Snapshot: {url}"
            | _ -> Effect.empty)

let () =
    let effects =
        [ Bot.effects
          Notifications.effects
          SubWorker.effects
          SnapshotWorker.effects ]
        |> Stream.mergeAll Effect.join
        |> Stream.accum Effect.empty (fun newEffs _ -> newEffs)

    (*  *)

    let mutable cmdLog = []

    let send msg target =
        let mutable localLog: obj list = []

        let _setEffects =
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

            StreamSinkFx.sendEventImpl <- fun _f _ -> ()

        RandomApi.randomProducer
        |> CellSink.send (RandomApi.randomProducer |> Cell.sample |> Generator.next |> snd)

        StreamSink.send msg target
        Effect.unsafeRun (Cell.sample effects)

        cmdLog <- cmdLog @ [ localLog ]

    (* Test *)

    send ("y2k", "/ls") TelegramApi.telegramMessageProducer
    send ("y2k", "/add https://g.com/") TelegramApi.telegramMessageProducer
    send ("y2k", "/ls") TelegramApi.telegramMessageProducer

    send
        (SubWorker.DownloadCompleted(Guid.Parse "0f156e9c-7f59-1219-9165-7367b8185e1f", "y2k", "https://g.com/", [||]))
        SubWorker.downloadCompleteProducer

    send ("y2k", "/ls") TelegramApi.telegramMessageProducer

    send () CommonFx.timerTicked

    send (Guid.Parse "43438761-3309-0746-0257-c2238a0ab02e", [||]) SnapshotWorker.downloadCompleted

    (* Assert *)

    assertEffect
        cmdLog
        """Gx8HAIzDdGMvl6slHlRNEtji/IX+fEAjLeCszbVSNhOfEpxGdNztIf7z/J8LcOAj3RtDNUXSQGaziLookEM6sGM80T8722wMEnv1iJp16lmfsYInADbArzoACNJFi6OeAuP1ab19pHM8NaSrdUvfuDXUGlfq2rgQQfaeXzxLdpjiyANo8VsdUbT/7O2egcgu758VVATGTXf9OT6KnBMJ7wdNRNgaHEz8i6qwTWfK+e+IjbRuHTP5jY0klYwUpbPktfNFkMGu5QaNWma+9cInmldo3r8fJxRjkftUn8VEKBXIUmE+gWKXby80adyHr9dmjdRxyFd0/dEYbXTwTpLWIpLwxpFQ1lNWSockUiHUGuHqq8Pnk4rVlxY5ixhideVzUCr91ZBC1dc6qM9J65Ju77k8bP1YS+u0FolyiGvy61Umk0SmFIzXXsjlfM63AuQEKsG4lzLIBKXc4mj5GQ=="""
