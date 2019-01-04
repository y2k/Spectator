module Spectator.Worker.App

open Spectator.Core

module R = Spectator.Server.App.Repository
module I = Spectator.Worker.Infrastructure

module Domain =
    let uriWithFlagsToCommand' userId uri isRss =
        match isRss with
        | true -> userId, uri, Provider.Rss
        | false -> userId, uri, Provider.Invalid

module Services =
    let private subWithFlag (x : NewSubscription) = RssParser.isValid x.uri >>- fun isValid -> x.userId, x.uri, isValid

    let createNewSubscription db newScription =
        newScription
        |> subWithFlag
        |> Async.map3 Domain.uriWithFlagsToCommand'
        >>= fun (userId, uri, p) -> R.createSubscription db userId uri p

    let createNewSubscriptions db = R.getNewSubscriptions' db |> Async.bindAll (createNewSubscription db)
    let private getNodesWithSubscription (x : Subscription) = RssParser.getNodes x.uri >>- fun snaps -> snaps, x

    let loadNewSnapshot db =
        R.getSubscriptions' db >>- List.filter (fun x -> x.provider = Provider.Rss)
        |> Async.bindAll getNodesWithSubscription
        |> Async.bindAll (fun (snapshots, subId) -> R.addSnapshotsForSubscription db snapshots)
        >>- ignore

let start db =
    async {
        printfn "Start worker..."
        Services.createNewSubscriptions db
        |> Async.next (Services.loadNewSnapshot db)
        |> I.executeInLoop 10000
    }

[<EntryPoint>]
let main _ = failwith "TODO"
