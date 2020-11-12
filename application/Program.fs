module Spectator.Application

open Spectator
open Spectator.Core

module N = Notifications
module W = Worker.App
module B = Bot.App
module P = Store.Persistent
module T = Tea.Persistent
module TP = Tea.Tea

let workerMainSub parsers =
    let parserIds =
        parsers |> List.map (fun (id, _, _) -> id)

    let loadSubscriptions (pid, u) =
        parsers
        |> List.find (fun (id, _, _) -> id = pid)
        |> fun (_, ls, _) -> ls u |> Async.catch

    let loadSnapshots (pid, u) =
        parsers
        |> List.find (fun (id, _, _) -> id = pid)
        |> fun (_, _, ls) -> ls u |> Async.catch

    W.Subscriptions.main parserIds loadSubscriptions

let workerMainSnap parsers =
    let loadSubscriptions (pid, u) =
        parsers
        |> List.find (fun (id, _, _) -> id = pid)
        |> fun (_, ls, _) -> ls u |> Async.catch

    let loadSnapshots (pid, u) =
        parsers
        |> List.find (fun (id, _, _) -> id = pid)
        |> fun (_, _, ls) -> ls u |> Async.catch

    W.Snapshots.main loadSnapshots

let mkApplication sendToTelegram readFromTelegram db downloadString enableLogs =
    (* Worker.TelegramParser.create config.restTelegramPassword config.restTelegramBaseUrl *)
    (* Worker.HtmlProvider.create config.filesDir *)

    let parsers =
        [ (Worker.RssParser.create downloadString) ]

    async {
        printfn "Started..."

        let! (botState, workerState, notifyState) =
            T.restoreState (P.applyObj db) (B.emptyState, W.Subscriptions.emptyState, N.emptyState) (fun (s1, s2, s3) e ->
                B.restore s1 e, W.Subscriptions.restore s2 e, N.restore s3 e)

        let store = TP.init ()

        let logTasks =
            if enableLogs then
                ignore (TP.make store () (fun _ e -> printfn "LOG event ::\n%O" e))
                [ TP.make store HealthCheck.State.Empty HealthCheck.updatePing
                  |> HealthCheck.main HealthCheck.startServer HealthCheck.sendText ]
            else
                []

        do! Async.loopAll [ yield! logTasks
                            yield workerMainSub parsers (TP.make store workerState W.Subscriptions.restore)
                            yield workerMainSnap parsers (TP.make store workerState W.Snapshots.restore)
                            yield B.main readFromTelegram sendToTelegram (TP.make store botState B.restore)
                            yield N.main sendToTelegram (TP.make store notifyState N.restore)
                            yield T.main (P.applyEvent db) (TP.make store T.initState T.restore) ]
    }

[<EntryPoint>]
let main args =
    let config = Config.readConfig args.[0]

    let sendToTelegramSingle =
        Telegram.sendToTelegramSingle config.telegramToken

    let readMessage =
        Telegram.readMessage config.telegramToken

    let db =
        Store.MongoDb.make config.mongoDomain "spectator"

    mkApplication sendToTelegramSingle readMessage db Worker.RssParser.Http.download true
    |> Async.RunSynchronously
    0
