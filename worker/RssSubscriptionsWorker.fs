module Spectator.Worker.RssSubscriptionsWorker

open System
open Spectator.Core

module RssParser = RssParser.Parser

type State =
    { newSubscriptions: NewSubscription list }
    static member empty = { newSubscriptions = [] }

let handleStateCmd (state: State) (cmd: Command) =
    let removeNewSubs (subscriptions: NewSubscription list) sids =
        subscriptions
        |> List.filter (fun s -> not <| List.contains s.id sids)

    match cmd with
    | :? SubscriptionRemoved as SubscriptionRemoved (sids, nsids) ->
        { state with newSubscriptions = removeNewSubs state.newSubscriptions nsids }
    | :? NewSubscriptionCreated as (NewSubscriptionCreated ns) ->
        { state with newSubscriptions = ns :: state.newSubscriptions }
    | _ -> state

type private DownloadCompleted =
    | DownloadCompleted of Uri * Result<byte [], exn>
    interface Event

let private mkSubscription (newSub: NewSubscription) (p: PluginId) : Subscription =
    { id = TypedId.wrap <| Guid.NewGuid()
      userId = newSub.userId
      provider = p
      uri = newSub.uri
      filter = newSub.filter }

let handleEvent (state: State) (e: Event) : Command list =
    match e with
    | :? TimerTicked ->
        state.newSubscriptions
        |> List.map (fun ns -> DownloadHttp(ns.uri, (fun data -> DownloadCompleted(ns.uri, data))))
    | :? DownloadCompleted as DownloadCompleted (uri, (Ok data)) ->
        let isRss = RssParser.isValid (Text.Encoding.UTF8.GetString data)

        state.newSubscriptions
        |> List.choose (fun ns ->
            if isRss && ns.uri = uri then
                [ SubscriptionCreated(mkSubscription ns RssParser.pluginId) :> Command
                  SubscriptionRemoved([], [ ns.id ]) ]
                |> Some
            else
                None)
        |> List.concat
    | _ -> []
