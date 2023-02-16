module Spectator.Application

open System
open Spectator
open Spectator.Core

module Persistent = Store.Persistent
module RssSubscriptionsWorker = Worker.RssSubscriptionsWorker
module RssSnapshotsWorker = Worker.RssSnapshotsWorker

let attachDomain () =
    Router.init
    |> fun router ->
        let state = AsyncRouter.make Bot.App.State.empty

        router
        |> Router.addEvent (AsyncRouter.decorateEventHandler state (Router.makeHandleEvent Bot.App.handleEvent))
        |> Router.addCommand (AsyncRouter.decorateCommandHandler state Bot.App.handleStateCmd)
    |> fun router ->
        let state = AsyncRouter.make Notifications.State.empty

        router
        |> Router.addEvent (AsyncRouter.decorateEventHandler state (Router.makeHandleEvent Notifications.handleEvent))
        |> Router.addCommand (AsyncRouter.decorateCommandHandler state Notifications.handleStateCmd)
    |> fun router ->
        let state = AsyncRouter.make RssSnapshotsWorker.State.empty

        router
        |> Router.addEvent (AsyncRouter.decorateEventHandler state RssSnapshotsWorker.handleEvent)
        |> Router.addCommand (AsyncRouter.decorateCommandHandler_ state RssSnapshotsWorker.handleStateCmd)
    |> fun router ->
        let state = AsyncRouter.make RssSubscriptionsWorker.State.empty

        router
        |> Router.addEvent (AsyncRouter.decorateEventHandler state RssSubscriptionsWorker.handleEvent)
        |> Router.addCommand (AsyncRouter.decorateCommandHandler_ state RssSubscriptionsWorker.handleStateCmd)

let runApplicaiton persCache router =
    async {
        do! Persistent.restoreCommands persCache (Router.makeCommandDispatch router)

        do!
            router
            |> Router.addCommand_ (Persistent.handleCommand persCache)
            |> Router.addEventGenerator (fun _ -> Persistent.main persCache)
            |> Router.start Initialize
    }

[<EntryPoint>]
let main _ =
    let filesDir = IO.Path.Combine(IO.Directory.GetCurrentDirectory(), "__data")
    let telegramToken = Environment.GetEnvironmentVariable "SPECTATOR_BOT_TOKEN"

    IO.Directory.CreateDirectory(filesDir) |> ignore

    let healthState = HealthCheck.init ()
    let persCache = Persistent.make (IO.Path.Combine(filesDir, "spectator.db"))

    attachDomain ()
    |> Router.addCommand (Https.handleCommand Https.download)
    // |> Router.addCommand SheduleGenerator.dispatchWithInterval
    // |> Router.addCommand SheduleGenerator.dispatchWithTimeout
    |> fun router ->
        router
        |> Router.addCommand_ Logger.logCommand
        |> Router.addEvent Logger.logEvent
    |> fun router ->
        router
        |> Router.addEvent (Router.makeHandleEvent_ HealthCheck.handleEvent)
        |> Router.addCommand_ (HealthCheck.handleCmd healthState)
        |> Router.addEventGenerator (HealthCheck.main healthState)
    |> fun router ->
        router
        |> Router.addCommand_ (TelegramEventAdapter.handleCommand (Telegram.sendToTelegramSingle telegramToken))
        |> Router.addEventGenerator (TelegramEventAdapter.generateEvents (Telegram.readMessage telegramToken))
    |> Router.addEventGenerator (Web.start)
    |> runApplicaiton persCache
    |> Async.RunSynchronously

    0
