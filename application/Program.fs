[<EntryPoint>]
let main _ =
    let db = MongoDB.Driver.MongoClient("mongodb://localhost").GetDatabase("spectator")
    Spectator.Worker.App.start db |> Async.Start
    Spectator.Bot.App.start db |> Async.Start
    System.Threading.Thread.Sleep -1
    0
