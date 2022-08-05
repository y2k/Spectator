module Spectator.Application

open System
open Spectator
open Spectator.Core

module StoreWrapper =
    module S = EventPersistent.Store
    let init = S.init

    [<Obsolete>]
    let make store initState update =
        let reduce = S.make store initState update

        { new IReducer<'state, Event> with
            member _.Invoke f =
                async {
                    let! oldState =
                        reduce (fun state ->
                            let (newState, events, _) = f state
                            newState, events)

                    let (_, _, result) = f oldState
                    return result
                } }

    let makeDispatch store handleEvent handleCmd =
        let dispatchStore = make store () (fun _ e -> handleEvent e |> List.iter handleCmd)

        fun (e: Event) ->
            dispatchStore.Invoke(fun _ -> (), [ e ], ())
            |> Async.Start

module Bot = Bot.App
module Persistent = Store.Persistent
module DatabaseAdapter = Store.DatabaseAdapter
module RssSubscriptionsWorker = Worker.RssSubscriptionsWorker
module RssSnapshotsWorker = Worker.RssSnapshotsWorker

let mkApplication
    sendToTelegram
    readFromTelegram
    downloadString
    isProduction
    syncSnapPeriod
    (connectionString: string)
    =
    async {
        let persCache = AsyncChannel.make ()
        let store: Event EventPersistent.Store.t = StoreWrapper.init ()
        let botState = StoreAtom.make ()
        let notifState = StoreAtom.make ()
        let rssSubState = StoreAtom.make ()
        let rssSnapState = StoreAtom.make ()

        let handleEvent e =
            [ yield! (StoreAtom.addStateCofx botState Bot.handleEvent) e
              yield! (StoreAtom.addStateCofx notifState Notifications.handleEvent) e
              yield! (StoreAtom.addStateCofx rssSubState RssSubscriptionsWorker.handleEvent) e
              yield! (StoreAtom.addStateCofx rssSnapState RssSnapshotsWorker.handleEvent) e ]

        let handleCommand dispatch cmd =
            Https.handleCommand downloadString dispatch cmd
            StoreAtom.handleCommand botState cmd
            TelegramEventAdapter.handleCommand sendToTelegram cmd
            StoreAtom.handleCommand notifState cmd
            StoreAtom.handleCommandFun notifState Notifications.handleStateCmd cmd
            StoreAtom.handleCommandFun rssSubState RssSubscriptionsWorker.handleStateCmd cmd
            StoreAtom.handleCommandFun rssSnapState RssSnapshotsWorker.handleStateCmd cmd

        let db = DatabaseAdapter.make connectionString
        do! Persistent.restore db (StoreWrapper.make store () (fun _ _ -> ()))

        let healthState = HealthCheck.init ()

        let (handleEvent, handleCommand, productionTasks) =
            if isProduction then
                let handleEvent e =
                    [ yield! handleEvent e
                      yield! HealthCheck.handleEvent e
                      yield! Logger.logEvent e ]

                let handleCommand dispatch cmd =
                    handleCommand dispatch cmd
                    HealthCheck.handleCmd healthState cmd
                    Logger.logCommand cmd

                handleEvent, handleCommand, [ HealthCheck.main healthState ]
            else
                handleEvent, (fun _ _ -> ()), []

        let mutable dispatch: Event -> unit = ignore
        dispatch <- StoreWrapper.makeDispatch store handleEvent (fun e -> handleCommand dispatch e)

        printfn "Started..."

        do!
            [ yield TimerAdapter.generateEvents dispatch
              yield TelegramEventAdapter.generateEvents readFromTelegram dispatch
              //   yield
              //       Async.delayAfter
              //           (TimeSpan.FromSeconds 2.0)
              //           (workerMainSub parsers (TP.make store SU.State.Empty SU.restore))
              //   yield Async.delayAfter syncSnapPeriod (workerMainSnap parsers (TP.make store SN.State.Empty SN.restore))
              yield Persistent.main db persCache
              yield! List.map (fun f -> f dispatch) productionTasks ]
            |> Async.loopAll
    }

[<EntryPoint>]
let main _ =
    let config =
        {| filesDir = IO.Path.Combine(IO.Directory.GetCurrentDirectory(), "__data")
           telegramToken = Environment.GetEnvironmentVariable "SPECTATOR_BOT_TOKEN"
           updateTimeMinutes = 1 |}

    IO.Directory.CreateDirectory(config.filesDir)
    |> ignore

    mkApplication
        (Telegram.sendToTelegramSingle config.telegramToken)
        (Telegram.readMessage config.telegramToken)
        Https.download
        true
        (TimeSpan.FromMinutes(float config.updateTimeMinutes))
        (IO.Path.Combine(config.filesDir, "spectator.db"))
    |> Async.RunSynchronously

    0
