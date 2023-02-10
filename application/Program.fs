module Spectator.Application

open System
open Spectator
open Spectator.Core

module Persistent = Store.Persistent
module RssSubscriptionsWorker = Worker.RssSubscriptionsWorker
module RssSnapshotsWorker = Worker.RssSnapshotsWorker

let runApplication persCache timerPeriod =
    Router.init
    |> Router.addStatefull (Router.makeHandleEvent Bot.App.handleEvent) Bot.App.handleStateCmd
    |> Router.addStatefull (Router.makeHandleEvent Notifications.handleEvent) Notifications.handleStateCmd
    |> Router.addStatefull_ RssSubscriptionsWorker.handleEvent RssSubscriptionsWorker.handleStateCmd
    |> Router.addStatefull_ RssSnapshotsWorker.handleEvent RssSnapshotsWorker.handleStateCmd
    |> Router.addEvent Persistent.handleEvent
    |> Router.addCommand_ (Persistent.handleCommand persCache)
    |> Router.addEventGenerator (TimerAdapter.generateEvents timerPeriod)
    |> Router.addEventGenerator (fun _ -> Persistent.main persCache)

[<EntryPoint>]
let main _ =
    let filesDir = IO.Path.Combine(IO.Directory.GetCurrentDirectory(), "__data")
    let telegramToken = Environment.GetEnvironmentVariable "SPECTATOR_BOT_TOKEN"
    let updateTimeMinutes = 1

    IO.Directory.CreateDirectory(filesDir) |> ignore

    let healthState = HealthCheck.init ()
    let persCache = Persistent.make (IO.Path.Combine(filesDir, "spectator.db"))

    runApplication persCache (TimeSpan.FromMinutes(float updateTimeMinutes))
    |> Router.addEvent Logger.logEvent
    |> Router.addEvent HealthCheck.handleEvent
    |> Router.addCommand_ Logger.logCommand
    |> Router.addCommand_ (TelegramEventAdapter.handleCommand (Telegram.sendToTelegramSingle telegramToken))
    |> Router.addCommand_ (HealthCheck.handleCmd healthState)
    |> Router.addCommand (Https.handleCommand Https.download)
    |> Router.addEventGenerator (HealthCheck.main healthState)
    |> Router.addEventGenerator (TelegramEventAdapter.generateEvents (Telegram.readMessage telegramToken))
    |> Router.addEventGenerator (Web.start)
    |> Router.start (Persistent.RestoreStateEvent persCache)
    |> Async.RunSynchronously

    0
