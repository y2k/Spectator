open Legivel.Serialization
open Spectator.Core
open Spectator.Worker

[<EntryPoint>]
let main _ =
    DependencyGraph.config <- 
        System.IO.File.ReadAllText "local-storage/settings.yml"
        |> Deserialize<DependencyGraph.Config>
        |> function | [ Succes { Data = x } ] -> x | _ -> failwith "error"

    let db = MongoDB.Driver
              .MongoClient(sprintf "mongodb://%s" DependencyGraph.config.mongoDomain)
              .GetDatabase("spectator")
    DependencyGraph.subscribeEff <- Spectator.Infrastructure.MongoCofx.subscribeQuery db
    DependencyGraph.dbEff <- { new IDbEff with member __.run f = Spectator.Infrastructure.MongoCofx.runCfx db f }

    let parsers =
        [ RssParser.RssParse
          TelegramParser.TelegramConnectorApiImpl
          HtmlProvider.HtmlParse ]

    [ Spectator.Worker.App.start parsers
      Spectator.Bot.App.start
      Spectator.Notifications.main ]
    |> Async.Parallel |> Async.RunSynchronously |> ignore
    0
