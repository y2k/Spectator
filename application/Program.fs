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
module N = Notifications
module W = Worker.App
module B = Bot.App
module P = Store.Persistent

let workerMain syncDelay initState parsers sendEvent readEvent =
    let executeEffect = W.executeEffect parsers sendEvent
    let update =
        parsers
        |> List.map @@ fun p -> p.id
        |> W.StateMachine.update syncDelay
    Tea.start initState (snd W.StateMachine.init) update W.StateMachine.EventReceived executeEffect readEvent

let notificationsMain initState sendToTelegramSingle readEvent =
    let executeEffect = N.executeEffect sendToTelegramSingle
    Tea.start initState (snd N.Domain.init) N.Domain.update N.Domain.EventReceived executeEffect readEvent

let botMain initState sendEvent sendToTelegram readFromTelegram readEvent =
    let executeEffect = B.executeEffect sendToTelegram readFromTelegram sendEvent
    Tea.start initState (snd B.Updater.init) B.Updater.update B.Updater.EventsReceived executeEffect readEvent

let persistentMain insert delete readEvent =
    let executeEffect = id
    Tea.start () [] (fun s _ -> (), List.map (P.executeEffect insert delete) s) List.singleton executeEffect readEvent

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
      let! (botState, workerState, notifyState) =
          Store.Persistent.restoreState forEach
              (B.emptyState, W.emptyState, N.emptyState)
              (fun (s1, s2, s3) e -> B.restore s1 e, W.restore s2 e, N.restore s3 e)

      do! [ logEvents (K.createReader group)
            persistentMain insert delete (K.createReader group)
            botMain botState (K.sendEvent group) sendToTelegramSingle readMessage (K.createReader group)
            notificationsMain notifyState sendToTelegramSingle (K.createReader group)
            workerMain (TimeSpan.FromMinutes <| float config.updateTimeMinutes) workerState parsers (K.sendEvent group) (K.createReader group) ]
          |> Async.Parallel |> Async.Ignore
    } |> Async.RunSynchronously
    0
