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
    let botState = StoreAtom.make ()
    let notifState = StoreAtom.make ()
    let rssSubState = StoreAtom.make ()
    let rssSnapState = StoreAtom.make ()

    let handleEvent e =
        [ yield! extEventHandler e
          yield! (StoreAtom.addStateCofx botState Bot.handleEvent) e
          yield! (StoreAtom.addStateCofx notifState Notifications.handleEvent) e
          yield! (StoreAtom.addStateCofx rssSubState RssSubscriptionsWorker.handleEvent) e
          yield! (StoreAtom.addStateCofx rssSnapState RssSnapshotsWorker.handleEvent) e
          yield! Persistent.handleEvent e ]

    let handleCommand dispatch cmd =
        extCommandHandler dispatch cmd
        StoreAtom.handleCommand botState cmd
        StoreAtom.handleCommand notifState cmd
        StoreAtom.handleCommandFun botState Bot.handleStateCmd cmd
        StoreAtom.handleCommandFun notifState Notifications.handleStateCmd cmd
        StoreAtom.handleCommandFun rssSubState RssSubscriptionsWorker.handleStateCmd cmd
        StoreAtom.handleCommandFun rssSnapState RssSnapshotsWorker.handleStateCmd cmd
        Persistent.handleCommand persCache cmd

    let dispatch = StoreWrapper.makeDispatch handleEvent handleCommand

    dispatch (Persistent.RestoreStateEvent persCache :> Event)

    printfn "Started..."

    [ yield TimerAdapter.generateEvents timerPeriod dispatch
      yield Persistent.main persCache
      yield! List.map (fun f -> f dispatch) productionTasks ]
    |> Async.loopAll

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
