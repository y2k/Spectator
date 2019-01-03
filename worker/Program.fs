module Spectator.Worker.App

open EasyNetQ
open Spectator
open Spectator.Core
open Spectator.Worker

module I = Spectator.Worker.Infrastructure

module Domain =
    let uriWithFlagsToCommand' userId uri isRss =
        match isRss with
        | true -> userId, uri, Provider.Rss
        | false -> userId, uri, Provider.Invalid
        |> fun (userId, uri, p) r -> CreateSubscription(userId, uri, p, r)

module Operations =
    let private subWithFlag (x : NewSubscription) = RssParser.isValid x.uri >>- fun isValid -> x.userId, x.uri, isValid

    let createNewSubscription' bus newScription =
        newScription
        |> subWithFlag
        |> Async.map3 Domain.uriWithFlagsToCommand'
        >>= Bus.reply bus

    let createNewSubscriptions' bus = Bus.reply bus GetAllNewSubscriptions |> Async.bindAll (createNewSubscription' bus)
    let private getNodesWithSubscription (x : Subscription) = RssParser.getNodes x.uri >>- fun snaps -> snaps, x

    let loadNewSnapshot' bus =
        Bus.reply bus GetAllSubscriptions >>- List.filter (fun x -> x.provider = Provider.Rss)
        |> Async.bindAll getNodesWithSubscription
        >>- List.map (fun (snapshots, subId) r -> AddSnapshotsForSubscription(snapshots, subId, r))
        |> Async.bindAll (Bus.reply bus)
        >>- ignore

let start bus =
    async {
        printfn "Start worker..."
        Operations.createNewSubscriptions' bus
        |> Async.next (Operations.loadNewSnapshot' bus)
        |> I.executeInLoop 10000
    }

[<EntryPoint>]
let main _ = failwith "TODO"
