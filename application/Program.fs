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

      do!
        [ Store.Persistent.main config.mongoDomain (K.createReader group)
          Bot.App.main botState (K.sendEvent group) (K.createReader group) repl
          Notifications.main (K.createReader group) sendToTelegramSingle
          Worker.App.main workerState parsers (K.sendEvent group) ]
        |> Async.Parallel |> Async.Ignore
    } |> Async.RunSynchronously
    0
