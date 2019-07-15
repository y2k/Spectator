open Spectator.Core
type E = System.Environment

[<EntryPoint>]
let main args =
    let env = EnvironmentConfig.Load(args.[0])

    sTelegramApi <- Spectator.Worker.TelegramParser.TelegramConnectorApiImpl()
    // Spectator.Worker.TelegramParser.test "kotlin_lang" |> Async.RunSynchronously

    // let db = E.GetEnvironmentVariable "SPECTATOR_MONGO_DOMAIN" ||| "localhost"
    //          |> fun host -> MongoDB.Driver.MongoClient(sprintf "mongodb://%s" host).GetDatabase("spectator")
    let db = MongoDB.Driver.MongoClient(sprintf "mongodb://%s" env.MongoDomain).GetDatabase("spectator")
    // let env = { admin = E.GetEnvironmentVariable "SPECTATOR_TELEGRAM_ADMIN"
    //             filesDir = E.GetEnvironmentVariable "SPECTATOR_FILES_DIR" }

    [ Spectator.Worker.App.start env db
      Spectator.Bot.App.start db env
      Spectator.Notifications.main env db ]
    |> Async.Parallel |> Async.RunSynchronously |> ignore
    0
