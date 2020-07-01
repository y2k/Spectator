open System
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

[<EntryPoint>]
let main args =
    let config : Config = readConfig args.[0]

    let parsers =
        [ Worker.RssParser.create
          Worker.TelegramParser.create config.restTelegramPassword config.restTelegramBaseUrl
          Worker.HtmlProvider.create config.filesDir ]

    let sendToTelegramSingle = Telegram.sendToTelegramSingle config.telegramToken
    let readMessage = Telegram.readMessage config.telegramToken
    let group = K.createGroup ()

    let db = Store.MongoDb.getDatabase config.mongoDomain "spectator"
    let forEach = { new Store.Persistent.IForEach with member _.invoke a b = Store.MongoDb.forEach db a b }
    let insert = { new Store.Persistent.IInsert with member _.invoke a b = Store.MongoDb.insert db a b }
    let delete = Store.MongoDb.delete db

    printfn "Started..."
    async {
      let! botState = Store.Persistent.restoreState forEach Bot.App.emptyState Bot.App.restore
      let! workerState = Store.Persistent.restoreState forEach Worker.App.emptyState Worker.App.restore
      let! notifyState = Store.Persistent.restoreState forEach Notifications.emptyState Notifications.restore

      do! [ logEvents (K.createReader group)
            Store.Persistent.main insert delete (K.createReader group)
            Bot.App.main botState (K.sendEvent group) (K.createReader group) sendToTelegramSingle readMessage
            Notifications.main notifyState (K.createReader group) sendToTelegramSingle
            Worker.App.main (TimeSpan.FromMinutes <| float config.updateTimeMinutes) workerState parsers (K.sendEvent group) (K.createReader group) ]
          |> Async.Parallel |> Async.Ignore
    } |> Async.RunSynchronously
    0
