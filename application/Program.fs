module Spectator.Application

open System
open Spectator
open Spectator.Core
open Y2k.EventBus

module Persistent = Store.Persistent
module RssSubscriptionsWorker = Worker.RssSubscriptionsWorker
module RssSnapshotsWorker = Worker.RssSnapshotsWorker

let attachDomain () =
    Router.init
    |> fun router ->
        let state = AsyncRouter.make Bot.App.State.empty

        router
        |> Router.addEvent (
            Bot.App.handleEvent
            |> RouterUtils.toCommon2
            |> AsyncRouter.decorateEventHandler state
        )
        |> Router.addCommand (AsyncRouter.makeCommandHandler state Bot.App.handleStateCmd)
    |> fun router ->
        let state = AsyncRouter.make Notifications.State.empty

        router
        |> Router.addEvent (
            Notifications.handleEvent
            |> RouterUtils.toCommon2
            |> AsyncRouter.decorateEventHandler state
        )
        |> Router.addCommand (AsyncRouter.makeCommandHandler state Notifications.handleStateCmd)
    |> fun router ->
        let state = AsyncRouter.make RssSnapshotsWorker.State.empty

        router
        |> Router.addEvent (
            RssSnapshotsWorker.handleEvent
            |> AsyncRouter.decorateEventHandler state
            |> EventLocker.decorateWithLock
        )
        |> Router.addCommand (AsyncRouter.makeCommandHandler state RssSnapshotsWorker.handleStateCmd)
    |> fun router ->
        let state = AsyncRouter.make RssSubscriptionsWorker.State.empty

        router
        |> Router.addEvent (
            RssSubscriptionsWorker.handleEvent
            |> AsyncRouter.decorateEventHandler state
            |> EventLocker.decorateWithLock
        )
        |> Router.addCommand (AsyncRouter.makeCommandHandler state RssSubscriptionsWorker.handleStateCmd)

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
    |> fun router ->
        router
        |> Router.addCommand_ Logger.logCommand
        |> Router.addEvent Logger.logEvent
    |> fun router ->
        router
        |> Router.addEvent (RouterUtils.toCommon HealthCheck.handleEvent)
        |> Router.addCommand_ (HealthCheck.handleCmd healthState)
        |> Router.addEventGenerator (HealthCheck.main healthState)
    |> fun router ->
        router
        |> Router.addCommand_ (TelegramEventAdapter.handleCommand (Telegram.sendToTelegramSingle telegramToken))
        |> Router.addEventGenerator (TelegramEventAdapter.generateEvents (Telegram.readMessage telegramToken))
    |> Router.addEventGenerator Web.start
    |> runApplicaiton persCache
    |> Async.RunSynchronously

    0
