module Spectator.Application

open System
open Spectator
open Spectator.Core

module Persistent = Store.Persistent
module RssSubscriptionsWorker = Worker.RssSubscriptionsWorker
module RssSnapshotsWorker = Worker.RssSnapshotsWorker

let attachDomain persCache =
    Router.init
    |> Router.addCommand_ (Persistent.handleCommand persCache)
    |> Router.addEvent (Router.makeHandleEvent_ Notifications.initialize)
    |> Router.addEventGenerator (fun _ -> Persistent.main persCache)
    |> Router.addStatefull (Router.makeHandleEvent Bot.App.handleEvent) Bot.App.handleStateCmd
    |> Router.addStatefull (Router.makeHandleEvent Notifications.handleEvent) Notifications.handleStateCmd
    |> Router.addStatefull_
        (RouterUtils.handleEvent
            (Guid.NewGuid())
            RssSnapshotsWorker.handleTimerEvent
            (Router.makeHandleEvent RssSnapshotsWorker.handleDownloadEvent))
        RssSnapshotsWorker.handleStateCmd
    |> Router.addStatefull_
        (RouterUtils.handleEvent
            (Guid.NewGuid())
            RssSubscriptionsWorker.handleTimerEvent
            (Router.makeHandleEvent RssSubscriptionsWorker.handleDownloadEvent))
        RssSubscriptionsWorker.handleStateCmd

let runApplicaiton persCache router =
    async {
        do! Persistent.restoreCommand persCache (Router.makeCommandDispatch router)
        router |> Router.start Initialize |> Async.RunSynchronously
    }

[<EntryPoint>]
let main _ =
    let filesDir = IO.Path.Combine(IO.Directory.GetCurrentDirectory(), "__data")
    let telegramToken = Environment.GetEnvironmentVariable "SPECTATOR_BOT_TOKEN"

    IO.Directory.CreateDirectory(filesDir) |> ignore

    let healthState = HealthCheck.init ()
    let persCache = Persistent.make (IO.Path.Combine(filesDir, "spectator.db"))

    let router =
        attachDomain persCache
        |> Router.addCommand (Https.handleCommand Https.download)
        |> Router.addCommand SheduleGenerator.dispatchWithInterval
        |> Router.addCommand SheduleGenerator.dispatchWithTimeout
        |> Router.addCommand_ (HealthCheck.handleCmd healthState)
        |> Router.addCommand_ (TelegramEventAdapter.handleCommand (Telegram.sendToTelegramSingle telegramToken))
        |> Router.addCommand_ Logger.logCommand
        |> Router.addEvent (Router.makeHandleEvent_ HealthCheck.handleEvent)
        |> Router.addEvent Logger.logEvent
        |> Router.addEventGenerator (HealthCheck.main healthState)
        |> Router.addEventGenerator (TelegramEventAdapter.generateEvents (Telegram.readMessage telegramToken))
        |> Router.addEventGenerator (Web.start)

    router |> runApplicaiton persCache |> Async.RunSynchronously

    0
