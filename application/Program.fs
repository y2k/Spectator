open Spectator.Core
open Legivel.Serialization

[<EntryPoint>]
let main _ =
    let env =
        System.IO.File.ReadAllText "local-storage/settings.yml"
        |> Deserialize<EnvironmentConfig.Root>
        |> function | [ Succes { Data = x } ] -> x | _ -> failwith "error"

    let deps = { telegram = Spectator.Worker.TelegramParser.TelegramConnectorApiImpl }
    let db = MongoDB.Driver.MongoClient(sprintf "mongodb://%s" env.MongoDomain).GetDatabase("spectator")

    [ Spectator.Worker.App.start env db
      Spectator.Bot.App.start deps db env
      Spectator.Notifications.main env db ]
    |> Async.Parallel |> Async.RunSynchronously |> ignore
    0
