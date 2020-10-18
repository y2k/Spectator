module Spectator.Application

open Spectator
open Spectator.Core

let readConfig path =
    System.IO.File.ReadAllText path
    |> Legivel.Serialization.Deserialize
    |> function [ Legivel.Serialization.Succes { Legivel.Serialization.Data = x } ] -> x | e -> failwithf "Can't parse config: %O" e

type Config =
    { filesDir : string
      mongoDomain : string
      restTelegramPassword : string
      restTelegramBaseUrl : string
      telegramToken : string
      updateTimeMinutes : int }

module N = Notifications
module W = Worker.App
module B = Bot.App
module P = Store.Persistent

let mkLog : Tea.t<Events> =
    Tea.make () (fun _ e -> printfn "Send event ::\n%O" e) (fun _ -> Async.Sleep 1_000)

let notificationsMain initState sendToTelegramSingle =
    Tea.make initState N.restore (N.main sendToTelegramSingle)

let workerMainSub initState parsers =
    let parserIds = parsers |> List.map @@ fun (id, _, _) -> id
    let loadSubscriptions (pid, u) =
        parsers
        |> List.find (fun (id, _, _) -> id = pid)
        |> fun (_, ls, _) -> ls u |> Async.catch
    let loadSnapshots (pid, u) =
        parsers
        |> List.find (fun (id, _, _) -> id = pid)
        |> fun (_, _, ls) -> ls u |> Async.catch
    Tea.make initState W.Subscriptions.restore (W.Subscriptions.main parserIds loadSubscriptions)

let workerMainSnap initState parsers =
    let loadSubscriptions (pid, u) =
        parsers
        |> List.find (fun (id, _, _) -> id = pid)
        |> fun (_, ls, _) -> ls u |> Async.catch
    let loadSnapshots (pid, u) =
        parsers
        |> List.find (fun (id, _, _) -> id = pid)
        |> fun (_, _, ls) -> ls u |> Async.catch
    Tea.make initState W.Snapshots.restore (W.Snapshots.main loadSnapshots)

let botMain initState sendToTelegram readFromTelegram =
    Tea.make initState B.restore (B.main readFromTelegram sendToTelegram)

let persistentMain insert delete =
    Tea.make P.initState P.restore (P.main insert delete)

let mkApplication sendToTelegram readFromTelegram mkPersistent restoreState downloadString enableLogs =
    let parsers =
        [ (Worker.RssParser.create downloadString)
          (* Worker.TelegramParser.create config.restTelegramPassword config.restTelegramBaseUrl *)
          (* Worker.HtmlProvider.create config.filesDir *) ]

    async {
        printfn "Started..."

        let! (botState, workerState, notifyState) =
            restoreState
                (B.emptyState, W.Subscriptions.emptyState, N.emptyState)
                (fun (s1, s2, s3) e -> B.restore s1 e, W.Subscriptions.restore s2 e, N.restore s3 e)

        do! Tea.run [
              if enableLogs then
                  yield mkLog
                  yield HealthCheck.main Tea.make HealthCheck.startServer HealthCheck.sendText
              yield mkPersistent
              yield botMain botState sendToTelegram readFromTelegram
              yield notificationsMain notifyState sendToTelegram
              yield workerMainSub workerState parsers
              yield workerMainSnap workerState parsers ]
    }

[<EntryPoint>]
let main args =
    let config: Config = readConfig args.[0]
    let sendToTelegramSingle = Telegram.sendToTelegramSingle config.telegramToken
    let readMessage = Telegram.readMessage config.telegramToken

    let db = Store.MongoDb.getDatabase config.mongoDomain "spectator"
    let forEach = { new Store.Persistent.IForEach with member _.invoke a b = Store.MongoDb.forEach db a b }
    let insert = { new Store.Persistent.IInsert with member _.invoke a b = Store.MongoDb.insert db a b }
    let delete = Store.MongoDb.delete db

    let restoreState = Store.Persistent.restoreState forEach
    let mkPersistent = persistentMain insert delete

    mkApplication sendToTelegramSingle readMessage mkPersistent restoreState Worker.RssParser.Http.download true
    |> Async.RunSynchronously
    0
