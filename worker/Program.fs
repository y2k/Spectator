module Spectator.Worker.App

open System
open Spectator.Core

type IParser = Spectator.Worker.HtmlProvider.IParse

module Domain =
    type Eff<'a, 'b> =
        { before : CoEffectDb -> PluginId list -> (PluginId * 'a) list
          effect : IParser -> 'a -> 'b Async
          after : CoEffectDb -> ((PluginId * 'a) * 'b) list -> CoEffectDb }

    let mkNewSubscriptions =
        let loadNewSubs (db : CoEffectDb) parserIds =
            db.newSubscriptions
            |> List.map (fun x -> x.uri)
            |> List.allPairs parserIds

        let saveSubs db subResps =
            let toSubscription (newSub : NewSubscription) =
                let providerId =
                    subResps
                    |> List.tryPick ^ fun ((id, uri), suc) ->
                        if suc && uri = newSub.uri then Some id else None
                { id = SubscriptionId.Empty
                  userId = newSub.userId
                  provider = providerId |> Option.defaultValue PluginId.Empty
                  uri = newSub.uri
                  filter = newSub.filter }

            let subs = db.subscriptions @ (List.map toSubscription db.newSubscriptions)
            { db with
                  subscriptions = subs |> List.filter ^ fun x -> x.provider <> PluginId.Empty
                  newSubscriptions = db.newSubscriptions |> List.exceptBy subs ^ fun ns sub -> ns.uri = sub.uri }

        { before = loadNewSubs
          effect = fun p -> p.isValid
          after = saveSubs }

    let mkNewSnapshots =
        let loadSnapshots (db : CoEffectDb) _ =
            db.subscriptions |> List.map ^ fun x -> x.provider, x.uri

        let saveSnapshots (db : CoEffectDb) snaps =
            db.subscriptions
            |> List.collect ^ fun sub ->
                snaps
                |> List.collect ^ fun ((p, u), snaps) ->
                    if p = sub.provider && u = sub.uri then snaps else []
                |> List.map ^ fun x -> { x with subscriptionId = sub.id }
            |> fun ss -> { db with snapshots = EventLog ss }

        { before = loadSnapshots
          effect = fun p -> p.getNodes
          after = saveSnapshots }

module Infrastructure =
    open Domain

    module M = Spectator.Infrastructure.MongoCofx

    let private apply parsers f requests =
        requests
        |> List.map ^ fun (p, url) ->
            let x : IParser = parsers |> List.find ^ fun x -> x.id = p
            f x url
        |> fun tasks -> Async.Parallel(tasks, 3)
        >>- (List.ofArray >> List.zip requests)

    let runFx mdb (parsers : IParser list) eff =
        let parserIds = parsers |> List.map ^ fun p -> p.id

        let runCfx0 f =
            let fixId id =
                if id = Guid.Empty then Guid.NewGuid() else id
            M.runCfx0 mdb ^ fun db ->
                let db = f db
                { db with subscriptions = db.subscriptions |> List.map ^ fun x -> { x with id = fixId x.id } }
        M.runCfx mdb ^ fun db -> db, eff.before db parserIds
        >>= apply parsers eff.effect
        >>= fun results -> runCfx0 ^ fun db -> eff.after db results

let start (parsers : HtmlProvider.IParse list) mdb =
    async {
        let log = Spectator.Infrastructure.Log.log
        let runFx eff = Infrastructure.runFx mdb parsers eff
        while true do
            log "Start syncing..."

            do! runFx Domain.mkNewSubscriptions
            do! runFx Domain.mkNewSnapshots

            log "End syncing, waiting..."
            do! Async.Sleep 600_000
    }
