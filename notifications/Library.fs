module Spectator.Notifications

open Spectator.Core
module M = Spectator.Infrastructure.MongoCofx
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

    let private getUpdates (subscriptions : Subscription list) snaps =
        subscriptions
        |> List.map ^ fun sub ->
            sub,
            snaps
            |> List.filter ^ fun x ->
                x.subscriptionId = sub.id && matchContent x.title sub.filter
        |> List.filter ^ fun (_, snaps) -> snaps |> (List.isEmpty >> not)

    let handleUpdate db =
        let botCommands =
            getUpdates db.subscriptions db.snapshots.unwrap
            |> List.map ^ fun (sub, snaps) -> sub.userId, mkMessage sub snaps
        botCommands

let main env mdb =
    let sendToTelegramBroadcast botCommands =
        botCommands
        |> List.map ^ uncurry (Bot.sendToTelegramSingle env)
        |> Async.Parallel
        >>- Array.filter ^ (<>) Bot.SuccessResponse
        >>- Array.iter ^ (sprintf "LOG :: can't send message %O" >> L.log)

    M.subscribeQuery mdb ^ fun db ->
        Domain.handleUpdate db
        |> sendToTelegramBroadcast
