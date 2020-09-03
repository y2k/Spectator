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

let logEvents logReader =
    async {
        while true do
            let! e = logReader
            printfn "Send event ::\n%O" e
    }

module K = Store.MiniKafka
module N = Notifications
module W = Worker.App
module B = Bot.App
module P = Store.Persistent

let notificationsMain initState sendToTelegramSingle sendEvent readEvent =
    Tea.runMain readEvent sendEvent initState N.restore (N.main sendToTelegramSingle)

let workerMain initState parsers sendEvent readEvent =
    let parserIds = parsers |> List.map @@ fun (id, _, _) -> id
    let loadSubscriptions (pid, u) =
        parsers
        |> List.find (fun (id, _, _) -> id = pid)
        |> fun (_, ls, _) -> ls u |> Async.catch
    let loadSnapshots (pid, u) =
        parsers
        |> List.find (fun (id, _, _) -> id = pid)
        |> fun (_, _, ls) -> ls u |> Async.catch
    Tea.runMain readEvent sendEvent initState W.restore (W.main parserIds loadSubscriptions loadSnapshots)

let botMain initState sendEvent sendToTelegram readFromTelegram readEvent =
    Tea.runMain readEvent sendEvent initState B.restore (B.main readFromTelegram sendToTelegram)

let persistentMain insert delete readEvent =
    let executeEffect = id
    Tea.start () [] (fun s _ -> (), List.map (P.executeEffect insert delete) s) List.singleton executeEffect readEvent

let mkApplication sendToTelegram readFromTelegram mkPersistent restoreState downloadString =
    let parsers =
        [ (Worker.RssParser.create downloadString)
          (* Worker.TelegramParser.create config.restTelegramPassword config.restTelegramBaseUrl *)
          (* Worker.HtmlProvider.create config.filesDir *) ]

    let group = K.createGroup ()

    async {
        printfn "Started..."

        let! (botState, workerState, notifyState) =
            restoreState
                (B.emptyState, W.emptyState, N.emptyState)
                (fun (s1, s2, s3) e -> B.restore s1 e, W.restore s2 e, N.restore s3 e)

        do! [ logEvents (K.createReader group)
              mkPersistent (K.createReader group)
              botMain botState (K.sendEvent group) sendToTelegram readFromTelegram (K.createReader group)
              notificationsMain notifyState sendToTelegram (K.sendEvent group) (K.createReader group)
              workerMain workerState parsers (K.sendEvent group) (K.createReader group) ]
            |> Async.Parallel |> Async.Ignore
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

    mkApplication sendToTelegramSingle readMessage mkPersistent restoreState Worker.RssParser.Http.download
    |> Async.RunSynchronously
    0
