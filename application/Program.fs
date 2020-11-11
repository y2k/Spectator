module Spectator.Application

open Spectator
open Spectator.Core

let readConfig path =
    System.IO.File.ReadAllText path
    |> Legivel.Serialization.Deserialize
    |> function
    | [ Legivel.Serialization.Succes { Legivel.Serialization.Data = x } ] -> x
    | e -> failwithf "Can't parse config: %O" e

type Config =
    { filesDir: string
      mongoDomain: string
      restTelegramPassword: string
      restTelegramBaseUrl: string
      telegramToken: string
      updateTimeMinutes: int }

module N = Notifications
module W = Worker.App
module B = Bot.App
module P = Store.Persistent
module T = Spectator.Core.Tea.Persistent

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

module Tea = Spectator.Core.Tea.Tea

let mkApplication sendToTelegram readFromTelegram mkPersistent restoreState downloadString enableLogs =
    (* Worker.TelegramParser.create config.restTelegramPassword config.restTelegramBaseUrl *)
    (* Worker.HtmlProvider.create config.filesDir *)

    let parsers =
        [ (Worker.RssParser.create downloadString) ]

    async {
        printfn "Started..."

        let! (botState, workerState, notifyState) =
            restoreState (B.emptyState, W.Subscriptions.emptyState, N.emptyState) (fun (s1, s2, s3) e ->
                B.restore s1 e, W.Subscriptions.restore s2 e, N.restore s3 e)

        let store = Tea.init ()

        let logTasks =
            if enableLogs then
                ignore (Tea.make store () (fun _ e -> printfn "LOG event ::\n%O" e))
                [ Tea.make store HealthCheck.State.Empty HealthCheck.updatePing
                  |> HealthCheck.main HealthCheck.startServer HealthCheck.sendText ]
            else
                []

        do! Async.loopAll [ yield! logTasks
                            yield workerMainSub parsers (Tea.make store workerState W.Subscriptions.restore)
                            yield workerMainSnap parsers (Tea.make store workerState W.Snapshots.restore)
                            yield B.main readFromTelegram sendToTelegram (Tea.make store botState B.restore)
                            yield N.main sendToTelegram (Tea.make store notifyState N.restore)
                            yield mkPersistent (Tea.make store T.initState T.restore) ]
    }

[<EntryPoint>]
let main args =
    let config: Config = readConfig args.[0]

    let sendToTelegramSingle =
        Telegram.sendToTelegramSingle config.telegramToken

    let readMessage =
        Telegram.readMessage config.telegramToken

    let db =
        Store.MongoDb.getDatabase config.mongoDomain "spectator"

    let forEach =
        { new T.IForEach with
            member _.invoke a b = Store.MongoDb.forEach db a b }

    let insert =
        { new T.IInsert with
            member _.invoke a b = Store.MongoDb.insert db a b }

    let delete = Store.MongoDb.delete db

    let restoreState = P.restoreState forEach
    let mkPersistent = P.main insert delete

    mkApplication sendToTelegramSingle readMessage mkPersistent restoreState Worker.RssParser.Http.download true
    |> Async.RunSynchronously
    0
