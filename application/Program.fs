open Spectator.Core

type E = System.Environment
module C = Spectator.Core

[<EntryPoint>]
let main _ =
    // C.sTelegramApi <- Spectator.Worker.TelegramParser.TelegramConnectorApiImpl()
    // Spectator.Worker.TelegramParser.test "kotlin_lang" |> Async.RunSynchronously

    let db = E.GetEnvironmentVariable "SPECTATOR_MONGO_DOMAIN" ||| "localhost"
             |> fun host -> MongoDB.Driver.MongoClient(sprintf "mongodb://%s" host).GetDatabase("spectator")
    let env = { admin = E.GetEnvironmentVariable "SPECTATOR_TELEGRAM_ADMIN" }

    Spectator.Worker.App.start db |> Async.Start
    Spectator.Bot.App.start db env |> Async.Start
    Spectator.Pushes.main db |> Async.Start
    System.Threading.Thread.Sleep -1
    0
