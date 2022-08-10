module Spectator.Application

open System
open Spectator
open Spectator.Core

module Bot = Bot.App
module Persistent = Store.Persistent
module DatabaseAdapter = Store.DatabaseAdapter
module RssSubscriptionsWorker = Worker.RssSubscriptionsWorker
module RssSnapshotsWorker = Worker.RssSnapshotsWorker

let runApplication timerPeriod (connectionString: string) extEventHandler extCommandHandler productionTasks =
    let persCache = Persistent.make connectionString

    Router.init
    |> Router.addEvent extEventHandler
    |> Router.addCommand extCommandHandler
    |> Router.addStatefull Bot.handleEvent Bot.handleStateCmd
    |> Router.addStatefull Notifications.handleEvent Notifications.handleStateCmd
    |> Router.addStatefull_ RssSubscriptionsWorker.handleEvent RssSubscriptionsWorker.handleStateCmd
    |> Router.addStatefull_ RssSnapshotsWorker.handleEvent RssSnapshotsWorker.handleStateCmd
    |> Router.addEvent Persistent.handleEvent
    |> Router.addCommand_ (Persistent.handleCommand persCache)
    |> Router.addEventGenerator (TimerAdapter.generateEvents timerPeriod)
    |> Router.addEventGenerators productionTasks
    |> Router.addEventGenerator (fun _ -> Persistent.main persCache)
    |> Router.start (Persistent.RestoreStateEvent persCache)

[<EntryPoint>]
let main _ =
    let config =
        {| filesDir = IO.Path.Combine(IO.Directory.GetCurrentDirectory(), "__data")
           telegramToken = Environment.GetEnvironmentVariable "SPECTATOR_BOT_TOKEN"
           updateTimeMinutes = 1 |}

    IO.Directory.CreateDirectory(config.filesDir)
    |> ignore

    let healthState = HealthCheck.init ()

    let handleEvent e =
        [ yield! Logger.logEvent e
          yield! HealthCheck.handleEvent e ]

    let handleCommand dispatch cmd =
        Logger.logCommand cmd
        TelegramEventAdapter.handleCommand (Telegram.sendToTelegramSingle config.telegramToken) cmd
        HealthCheck.handleCmd healthState cmd
        Https.handleCommand Https.download dispatch cmd

    runApplication
        (TimeSpan.FromMinutes(float config.updateTimeMinutes))
        (IO.Path.Combine(config.filesDir, "spectator.db"))
        handleEvent
        handleCommand
        [ HealthCheck.main healthState
          TelegramEventAdapter.generateEvents (Telegram.readMessage config.telegramToken) ]
    |> Async.RunSynchronously

    0
