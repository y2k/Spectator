open MongoDB.Driver

[<EntryPoint>]
let main _ =
    let db = MongoClient("mongodb://localhost").GetDatabase("spectator")
    Spectator.Worker.App.start db |> Async.Start
    Spectator.Bot.App.start db |> Async.Start
    0
