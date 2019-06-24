open Spectator.Core

[<EntryPoint>]
let main _ =
    let host = System.Environment.GetEnvironmentVariable "SPECTATOR_MONGO_DOMAIN" ||| "localhost"
    let db = MongoDB.Driver.MongoClient(sprintf "mongodb://%s" host).GetDatabase("spectator")

    Spectator.Worker.App.start db |> Async.Start
    Spectator.Bot.App.start db |> Async.Start
    System.Threading.Thread.Sleep -1
    0
