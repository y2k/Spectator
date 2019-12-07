open Legivel.Serialization
open Spectator.Core
open Spectator.Worker

[<EntryPoint>]
let main _ =
    let env =
        System.IO.File.ReadAllText "local-storage/settings.yml"
        |> Deserialize<EnvironmentConfig.Root>
        |> function | [ Succes { Data = x } ] -> x | _ -> failwith "error"

    let deps = { telegram = Spectator.Worker.TelegramParser.TelegramConnectorApiImpl }
    let db = MongoDB.Driver.MongoClient(sprintf "mongodb://%s" env.MongoDomain).GetDatabase("spectator")

    let parsers = Map.ofList [
        Provider.Rss, RssParser.RssParse
        Provider.Telegram, Spectator.Worker.TelegramParser.TelegramConnectorApiImpl :> HtmlProvider.IParse
        Provider.Html, HtmlProvider.HtmlParse(env) :> HtmlProvider.IParse ]

    [ Spectator.Worker.App.start parsers db
      Spectator.Bot.App.start deps db env
      Spectator.Notifications.main env db ]
    |> Async.Parallel |> Async.RunSynchronously |> ignore
    0
