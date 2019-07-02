module Spectator.Notifications

open MongoDB
open MongoDB.Driver
open Spectator.Core
open Telegram.Bot
open Telegram.Bot.Types
module I = Spectator.Infrastructure
type L = Spectator.Infrastructure.Log

module Domain =
    type R = System.Text.RegularExpressions.Regex

    let private mkMessage (sub : Subscription) (snaps : Snapshot list) : string =
        let prefix = sprintf "Update for <code>%O</code> (%i):" sub.uri (List.length snaps)
        snaps
        |> List.fold (fun s x -> sprintf "%s\n- <a href=\"%O\">%s</a>" s x.uri x.title) prefix

    let private matchContent content optRegex =
        if String.isNullOrEmpty optRegex then true
        else R.IsMatch(content, optRegex)

    let private getUpdates (subscriptions : Subscription list) snaps lastId =
        let actualSnaps = snaps |> List.takeWhile ^ fun x -> x.id <> lastId
        subscriptions
        |> List.map ^ fun sub ->
            sub,
            actualSnaps
            |> List.filter ^ fun x ->
                x.subscriptionId = sub.id && matchContent x.title sub.filter
        |> List.filter ^ fun (_, snaps) -> snaps |> (List.isEmpty >> not)

    let handleUpdate db snaps (topSnapshotIdOpt : string option) =
        if (List.isEmpty snaps) then [], topSnapshotIdOpt
        else if Option.isNone topSnapshotIdOpt then [], Some snaps.[0].id
        else
            let topSnapId = Option.get topSnapshotIdOpt
            let botCommands =
                getUpdates db.subscriptions snaps topSnapId
                |> List.map ^ fun (sub, snaps) -> sub.userId, mkMessage sub snaps
            botCommands, Some snaps.[0].id

module private Effects =
    let querySnapshots (mdb : IMongoDatabase) =
        mdb.GetCollection<Snapshot>("snapshots")
            .Find(FilterDefinition.op_Implicit "{}")
            .Sort(SortDefinition.op_Implicit "{$natural:-1}")
            .Limit(System.Nullable 100)
            .ToListAsync() |> Async.AwaitTask
        >>- Seq.toList

    let sendToTelegramBroadcast botCommands =
        botCommands
        |> List.map ^ uncurry Bot.sendToTelegramSingle
        |> Async.Parallel
        >>- Array.filter ^ (<>) Bot.SuccessResponse
        >>- Array.iter ^ (sprintf "LOG :: can't send message %O" >> L.log)

let main mdb = async {
    let topSnapshotId : string option ref = ref None
    while true do
        let! snaps = Effects.querySnapshots mdb
        let! (botCommands, newTopSnapId) =
            I.runCfx mdb ^ fun db -> db, Domain.handleUpdate db snaps !topSnapshotId

        topSnapshotId := newTopSnapId
        do! Effects.sendToTelegramBroadcast botCommands
        do! Async.Sleep 5_000 }
