module Spectator.Worker.TelegramWorker

open System
open Spectator.Core
open System.Text.RegularExpressions

let private PluginId = Guid.Parse("FA3B985E-466F-4C47-9222-CB090ED3FED2")
let private telegramRegex = Regex("https://t.me/([^/]+)/(\\d+)")

module Subscriptions =
    type State =
        { newSubs: NewSubscription list }
        static member empty = { newSubs = [] }


    let handleStateCmd (state: State) (cmd: Command) =
        match cmd with
        | :? NewSubscriptionCreated as NewSubscriptionCreated ns -> { state with newSubs = ns :: state.newSubs }
        | :? SubscriptionRemoved as SubscriptionRemoved (_, nsIds) ->
            { state with newSubs = List.filter (fun ns -> not <| List.contains ns.id nsIds) state.newSubs }
        | _ -> state

    let private mkSubscription (newSub: NewSubscription) : Subscription =
        { id = TypedId.wrap <| Guid.NewGuid()
          userId = newSub.userId
          provider = PluginId
          uri = newSub.uri
          filter = newSub.filter }

    let handleEvent (state: State) (e: Event) =
        match e with
        | :? TimerTicked ->
            state.newSubs
            |> List.filter (fun ns -> telegramRegex.IsMatch(ns.uri.AbsoluteUri))
            |> List.collect (fun ns ->
                [ SubscriptionCreated(mkSubscription ns) :> Command
                  SubscriptionRemoved([], [ ns.id ]) ])
        | _ -> []

module Snapshots =
    type State =
        { subscriptions: Subscription list }
        static member empty = { subscriptions = [] }

    let handleStateCmd state (e: Command) =
        match e with
        | :? SubscriptionCreated as SubscriptionCreated sub -> { state with subscriptions = sub :: state.subscriptions }
        | :? SubscriptionRemoved as SubscriptionRemoved (subIds, _) ->
            { state with subscriptions = List.filter (fun sub -> not <| List.contains sub.id subIds) state.subscriptions }
        | _ -> state

    let private makeWebUrl (uri: Uri) =
        let m = telegramRegex.Match(uri.AbsoluteUri)
        Uri(sprintf "https://t.me/%s/%s?embed=1" m.Groups[1].Value m.Groups[2].Value)

    type DownloadCompleted =
        | DownloadCompleted of sub: Subscription * result: Result<byte [], exn>
        interface Event

    let handleSnapshotsEvent (state: State) (e: Event) =
        match e with
        | :? TimerTicked ->
            state.subscriptions
            |> List.filter (fun sub -> sub.provider = PluginId)
            |> List.map (fun sub -> DownloadHttp(makeWebUrl sub.uri, (fun r -> DownloadCompleted(sub, r))))
        | :? DownloadCompleted as DownloadCompleted (sub, Ok bytes) -> failwith "???"
        | _ -> []
