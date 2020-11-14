module Spectator.Application

open Spectator
open Spectator.Core

module N = Notifications
module W = Worker.App
module B = Bot.App
module P = Store.Persistent
module TP = EventPersistent.Tea
module M = Store.MongoDb

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

let mkApplication sendToTelegram readFromTelegram downloadString enableLogs insert delete queryAll =
    (* Worker.TelegramParser.create config.restTelegramPassword config.restTelegramBaseUrl *)
    (* Worker.HtmlProvider.create config.filesDir *)

    let parsers =
        [ (Worker.RssParser.create downloadString) ]

    async {
        printfn "Restore state..."

        let store = TP.init ()

        let a =
            TP.make store W.Subscriptions.emptyState W.Subscriptions.restore

        let b =
            TP.make store W.Subscriptions.emptyState W.Snapshots.restore

        let c = TP.make store B.emptyState B.restore
        let d = TP.make store N.emptyState N.restore

        do! P.restore queryAll (TP.make store () (fun _ _ -> ()))

        let e = TP.make store P.State.Empty P.update

        let logTasks =
            if enableLogs then
                ignore (TP.make store () (fun _ e -> printfn "LOG event ::\n%O" e))

                [ TP.make store HealthCheck.State.Empty HealthCheck.updatePing
                  |> HealthCheck.main HealthCheck.startServer HealthCheck.sendText ]
            else
                []

        printfn "Started..."

        do! Async.loopAll [ yield! logTasks
                            yield workerMainSub parsers a
                            yield workerMainSnap parsers b
                            yield B.main readFromTelegram sendToTelegram c
                            yield N.main sendToTelegram d
                            yield P.main insert delete e ]
    }

[<EntryPoint>]
let main args =
    let config = Config.readConfig args.[0]

    let db =
        Store.MongoDb.make config.mongoDomain "spectator"

    mkApplication
        (Telegram.sendToTelegramSingle config.telegramToken)
        (Telegram.readMessage config.telegramToken)
        Worker.RssParser.Http.download
        true
        (M.insert db)
        (M.delete db)
        (M.queryAll db)
    |> Async.RunSynchronously

    0
