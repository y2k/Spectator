open Spectator.Core

type E = System.Environment

[<EntryPoint>]
let main _ =
    Spectator.Worker.TelegramParser.init()

    let host = E.GetEnvironmentVariable "SPECTATOR_MONGO_DOMAIN" ||| "localhost"
    let db = MongoDB.Driver.MongoClient(sprintf "mongodb://%s" host).GetDatabase("spectator")
    let env = { admin = E.GetEnvironmentVariable "SPECTATOR_TELEGRAM_ADMIN" }

    Spectator.Worker.App.start db |> Async.Start
    Spectator.Bot.App.start db env |> Async.Start
    Spectator.Pushes.main db |> Async.Start
    System.Threading.Thread.Sleep -1
    0
