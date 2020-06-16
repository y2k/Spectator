open System
open Spectator
open Spectator.Core

let readConfig path =
    System.IO.File.ReadAllText path
    |> Legivel.Serialization.Deserialize
    |> function [ Legivel.Serialization.Succes { Legivel.Serialization.Data = x } ] -> x | _ -> failwith "error"

type Config =
    { filesDir : string
      mongoDomain : string
      restTelegramPassword : string
      restTelegramBaseUrl : string
      telegramToken : string }

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
        [ Worker.RssParser.RssParse
          Worker.HtmlProvider.HtmlParse config.filesDir ]

    let repl = Telegram.repl config.telegramToken
    let sendToTelegramSingle = Telegram.sendToTelegramSingle config.telegramToken
    let group = K.createGroup ()

    async {
      let! botState = Store.Persistent.restoreState config.mongoDomain Bot.App.emptyState Bot.App.restore
      let! workerState = Store.Persistent.restoreState config.mongoDomain Worker.App.emptyState Worker.App.restore
      let! notifyState = Store.Persistent.restoreState config.mongoDomain Notifications.emptyState Notifications.restore

      do! [ logEvents (K.createReader group)
            Store.Persistent.main config.mongoDomain (K.createReader group)
            Bot.App.main botState (K.sendEvent group) (K.createReader group) repl
            Notifications.main notifyState (K.createReader group) sendToTelegramSingle
            Worker.App.main (TimeSpan.FromMinutes 1.) workerState parsers (K.sendEvent group) (K.createReader group) ]
          |> Async.Parallel |> Async.Ignore
    } |> Async.RunSynchronously
    0
