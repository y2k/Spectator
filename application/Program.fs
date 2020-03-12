open Legivel.Serialization
open Spectator.Core
open Spectator.Worker

let readConfig path =
    System.IO.File.ReadAllText path
    |> Deserialize<DependencyGraph.Config>
    |> function [ Succes { Data = x } ] -> x | _ -> failwith "error"

[<EntryPoint>]
let main args =
    DependencyGraph.config <- readConfig args.[0]

    let dbProvider = Spectator.Infrastructure.MongoCofx.mkProvider ()
    DependencyGraph.listenLogUpdates <- dbProvider.listen
    DependencyGraph.dbEff <- { new IDbEff with member __.run filter f = dbProvider.run filter f }

    let parsers =
        [ RssParser.RssParse
          HtmlProvider.HtmlParse ]

    [ Spectator.Worker.App.start parsers
      Spectator.Bot.App.main
      Spectator.Notifications.main ]
    |> Async.Parallel |> Async.RunSynchronously |> ignore
    0
