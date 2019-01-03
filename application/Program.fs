open System

[<EntryPoint>]
let main _ =
    let inbox = Spectator.Server.App.start()
    Spectator.Worker.App.start inbox |> Async.Start
    Spectator.Bot.App.start inbox |> Async.Start
    0
