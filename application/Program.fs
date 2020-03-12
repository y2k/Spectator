open Legivel.Serialization
open Spectator.Core
open Spectator.Worker

[<EntryPoint>]
let main args =
    DependencyGraph.config <- 
        System.IO.File.ReadAllText args.[0]
        |> Deserialize<DependencyGraph.Config>
        |> function [ Succes { Data = x } ] -> x | _ -> failwith "error"

    let db = Spectator.Infrastructure.MongoCofx.mkDatabase ()
    DependencyGraph.listenLogUpdates <- Spectator.Infrastructure.MongoCofx.subscribeQuery db
    DependencyGraph.dbEff <- 
        { new IDbEff with member __.run filter f = Spectator.Infrastructure.MongoCofx.runCfx filter db f }

    let parsers =
        [ RssParser.RssParse
          // TelegramParser.Parser
          HtmlProvider.HtmlParse ]

    [ Spectator.Worker.App.start parsers
      Spectator.Bot.App.main
      Spectator.Notifications.main ]
    |> Async.Parallel |> Async.RunSynchronously |> ignore
    0
