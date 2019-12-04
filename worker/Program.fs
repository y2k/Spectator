module Spectator.Worker.App

open Spectator.Core
type L = Spectator.Infrastructure.Log

module private Domain =
    open MongoDB.Bson
    open System
    module R = Spectator.Core.MongoCollections

    let mkSnapshotSaveCommands (db : CoEffectDb) (snaps : ((Provider * Uri) * Snapshot list) list) =
        db.subscriptions
        |> List.collect ^ fun sub ->
            snaps |> List.collect ^ fun ((p, u), x) ->
                if p = sub.provider && sub.uri = u then x else []
                |> List.map ^ fun x -> { x with subscriptionId = sub.id }
        |> fun ss -> { db with snapshots = LogList ss }

    let loadSnapshots (db : CoEffectDb) =
        db.subscriptions
        |> List.map (fun x -> x.provider, x.uri)

    let saveSubs (db : CoEffectDb) requests results =
        let toSubscription (newSub : NewSubscription) =
            let provider =
                requests
                |> List.zip results
                |> List.tryPick ^ fun (suc, (p, uri)) ->
                    if uri = newSub.uri && suc then Some p else None
            { id = System.Guid.NewGuid()
              userId = newSub.userId
              provider = provider |> Option.defaultValue Provider.Invalid
              uri = newSub.uri
              filter = newSub.filter }
        let ws = List.map toSubscription db.newSubscriptions
        { db with newSubscriptions = []; subscriptions = db.subscriptions @ ws }

    let loadNewSubs (db : CoEffectDb) =
        db.newSubscriptions
        |> List.map (fun x -> x.uri)
        |> List.allPairs [ Provider.Rss; Provider.Telegram ]

module private Effects =
    let isValid ps =
        ps
        |> List.map (fun (p, url) ->
            match p with
            | Provider.Rss -> RssParser.isValid url
            | Provider.Telegram -> sTelegramApi.isValid url
            | Provider.Html -> HtmlProvider.isValid url
            | _ -> failwithf "%O" p)
        |> Async.Parallel >>- fun xs -> List.ofArray xs

    let loadSnapshots env ps =
        ps
        |> List.map (fun (p, url) ->
            match p with
            | Provider.Rss -> RssParser.getNodes url
            | Provider.Telegram -> sTelegramApi.getNodes url
            | Provider.Html -> HtmlProvider.getNodes env url
            | _ -> failwithf "%O" p)
        |> Async.Parallel
        >>- fun xs -> List.zip ps (List.ofArray xs)

module M = Spectator.Infrastructure.MongoCofx

let start env mdb = async {
    while true do
        L.log "Start syncing..."

        let! subReqs = M.runCfx mdb ^ fun db -> db, Domain.loadNewSubs db
        L.log (sprintf "LOG :: new subscriptions requests %A" subReqs)

        let! subResps = Effects.isValid subReqs
        L.log ^ sprintf "LOG :: new subscriptions results %A" subResps

        do! M.runCfx mdb ^ fun db -> Domain.saveSubs db subReqs subResps, ()

        let! newSnapshots =
            M.runCfx mdb ^ fun db -> db, Domain.loadSnapshots db
            >>= Effects.loadSnapshots env
        L.log ^ sprintf "LOG :: new snapshots %A" newSnapshots

        do! M.runCfx mdb ^ fun db ->
                Domain.mkSnapshotSaveCommands db newSnapshots, ()

        L.log "End syncing, waiting..."
        do! Async.Sleep 600_000 }
