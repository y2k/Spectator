module Spectator.Worker.App

open System
open Spectator.Core
type IParser = HtmlProvider.IParse

module Domain =
    let toSubscription subResps (newSub : NewSubscription) =
        let providerId =
            subResps
            |> List.tryPick ^ fun ((id, uri), suc) ->
                match suc with
                | Ok suc -> if suc && uri = newSub.uri then Some id else None
                | Error _ -> None
        { id = TypedId.wrap Guid.Empty
          userId = newSub.userId
          provider = providerId |> Option.defaultValue Guid.Empty
          uri = newSub.uri
          filter = newSub.filter }

    let mkSnapshots responses subs =
        subs
        |> List.collect ^ fun sub ->
            responses
            |> List.collect ^ fun ((p, u), snaps) ->
                match snaps with
                | Ok snaps -> if p = sub.provider && u = sub.uri then snaps else []
                | Error _ -> []
            |> List.map ^ fun x -> { x with subscriptionId = sub.id }

    let removeSubs newSubscriptions (subs : Subscription list) =
        List.exceptBy subs (fun (ns : NewSubscription) sub -> ns.uri = sub.uri) newSubscriptions

module Services =
    type Request = PluginId * Uri
    type Msg = 
        | Init
        | MkSubscriptionsEnd of (Request * Result<bool, exn>) list
        | MkNewSnapshots 
        | MkNewSnapshotsEnd of (Request * Result<Snapshot list, exn>) list
    type 'a Cmd =
        | Delay of TimeSpan * (unit -> 'a)
        | LoadSubscriptions of Request list * (Result<bool, exn> list -> 'a)
        | LoadSnapshots of Request list * (Result<Snapshot list, exn> list -> 'a)

    let update parserIds msg (db : CoEffectDb) =
        match msg with
        | Init ->
            db,
            db.newSubscriptions
            |> List.map ^ fun x -> x.uri
            |> List.allPairs parserIds
            |> fun req -> [ LoadSubscriptions (req, fun resp -> MkSubscriptionsEnd (List.map2 pair req resp)) ]
        | MkSubscriptionsEnd subResps ->        
            let subs = db.subscriptions @ List.map (Domain.toSubscription subResps) db.newSubscriptions
            { db with
                subscriptions = List.filter (fun x -> x.provider <> Guid.Empty) subs
                newSubscriptions = Domain.removeSubs db.newSubscriptions subs }, 
            [ Delay (TimeSpan.Zero, always MkNewSnapshots) ]
        | MkNewSnapshots -> 
            db,
            db.subscriptions
            |> List.map ^ fun x -> x.provider, x.uri
            |> fun req -> [ LoadSnapshots (req, fun resp -> MkNewSnapshotsEnd (List.map2 pair req resp)) ]
        | MkNewSnapshotsEnd responses -> 
            db.subscriptions
            |> Domain.mkSnapshots responses
            |> fun snaps -> { db with snapshots = EventLog snaps }
            , [ Delay (TimeSpan.FromMinutes 5., always Init) ]

module Effects =
    let runPlugin f (parsers : HtmlProvider.IParse list) requests =
        requests
        |> List.map ^ fun (pluginId, uri) ->
            parsers
            |> List.find ^ fun p -> p.id = pluginId
            |> fun p -> f p uri
            |> Async.catch
        |> Async.Sequential
        >>- List.ofArray

let start (parsers : HtmlProvider.IParse list) =
    let parserIds = parsers |> List.map ^ fun p -> p.id
    let effects = function
        | Services.Delay (time, f) -> Async.Sleep (int time.TotalMilliseconds) >>- f
        | Services.LoadSubscriptions (requests, f) -> Effects.runPlugin (fun p uri -> p.isValid uri) parsers requests >>- f
        | Services.LoadSnapshots (requests, f) -> Effects.runPlugin (fun p uri -> p.getNodes uri) parsers requests >>- f
    Eff.runEffects effects (Services.update parserIds) Services.Init
