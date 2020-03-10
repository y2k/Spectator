module Spectator.Notifications

open Spectator.Core

type MessageCmd = { userId : string ; message : string }

module Domain =
    type R = System.Text.RegularExpressions.Regex

    let private mkMessage (sub : Subscription) (snaps : Snapshot list) : string =
        let addLine prev snap = sprintf "%s\n- <a href=\"%O\">%s</a>" prev snap.uri snap.title
        let prefix = sprintf "Update for <code>%O</code> (%i):" sub.uri (List.length snaps)
        List.fold addLine prefix snaps

    let private matchContent content optRegex =
        if String.isNullOrEmpty optRegex 
            then true
            else R.IsMatch(content, optRegex)

    let private getUpdates (subscriptions : Subscription list) snaps =
        let findSnapsForSub (sub : Subscription) =
            snaps
            |> List.filter ^ fun x ->
                x.subscriptionId = sub.id && matchContent x.title sub.filter

        subscriptions
        |> List.map ^ fun sub -> sub, findSnapsForSub sub
        |> List.filter ^ fun (_, snaps) -> List.isNotEmpty snaps

    let createNotificationMessages db =
        getUpdates db.subscriptions db.snapshots.unwrap
        |> List.map ^ fun (sub, snaps) -> { userId = sub.userId; message = mkMessage sub snaps }

let main =
    let sendToTelegramBroadcast messages =
        messages
        |> List.map ^ fun x -> Bot.sendToTelegramSingle x.userId x.message
        |> fun asyncSeq -> Async.Parallel (asyncSeq, maxDegreeOfParallelism = 5)
        >>- Array.filter ^ (<>) Bot.SuccessResponse
        >>- Array.iter ^ (sprintf "LOG :: can't send message %O" >> Log.log)

    DependencyGraph.listenLogUpdates ^ (Domain.createNotificationMessages >> sendToTelegramBroadcast)
