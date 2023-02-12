module Spectator.Worker.RssSubscriptionsWorker

open System
open Spectator.Core

module RssParser = RssParser.Parser

type State =
    { newSubscriptions: NewSubscription list }
    static member empty = { newSubscriptions = [] }

let handleStateCmd (state: State) (cmd: Command) =
    let removeNewSubs (subscriptions: NewSubscription list) sids =
        subscriptions |> List.filter (fun s -> not <| List.contains s.id sids)

    match cmd with
    | :? SubscriptionRemoved as SubscriptionRemoved (sids, nsids) ->
        { state with newSubscriptions = removeNewSubs state.newSubscriptions nsids }
    | :? NewSubscriptionCreated as (NewSubscriptionCreated ns) ->
        { state with newSubscriptions = ns :: state.newSubscriptions }
    | _ -> state

type private DownloadCompleted =
    | DownloadCompleted of Uri list * Result<byte[], exn> list
    interface Event

let private mkSubscription (newSub: NewSubscription) (p: PluginId) : Subscription =
    { id = TypedId.wrap <| Guid.NewGuid()
      userId = newSub.userId
      provider = p
      uri = newSub.uri
      filter = newSub.filter }

type RssSubscriptionsUpdate = RssSubscriptionsUpdate
    with
        interface Event

let handleEvent (state: State) (e: Event) : Command list =
    match e with
    | :? RssSubscriptionsUpdate ->
        state.newSubscriptions
        |> List.map (fun ns -> ns.uri)
        |> fun uris -> [ DownloadHttp(uris, (fun data -> DownloadCompleted(uris, data))) ]
    | :? DownloadCompleted as DownloadCompleted (uris, results) ->
        let responses =
            Seq.zip uris results
            |> Seq.choose (fun (k, v) ->
                match v with
                | Ok d -> Some(string k, d)
                | Error _ -> None)
            |> Map.ofSeq

        let isRss (data: byte[]) =
            RssParser.isValid (Text.Encoding.UTF8.GetString data)

        state.newSubscriptions
        |> List.choose (fun ns -> Map.tryFind (string ns.uri) responses |> Option.map (fun d -> ns, d))
        |> List.choose (fun (ns, data) ->
            if isRss data then
                [ SubscriptionCreated(mkSubscription ns RssParser.pluginId) :> Command
                  SubscriptionRemoved([], [ ns.id ]) ]
                |> Some
            else
                None)
        |> List.concat
        |> List.append [ DispatchWithTimeout(TimeSpan.FromMinutes 1, RssSubscriptionsUpdate) ]
    | _ -> []
