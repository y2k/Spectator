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

module SN = Worker.SnapshotsMain
module SU = Worker.SubscriptionsMain
module Bot = Bot.App
module Persistent = Store.Persistent
module DatabaseAdapter = Store.DatabaseAdapter
module TP = StoreWrapper

let workerMainSub parsers =
    let parserIds = parsers |> List.map (fun (id, _, _) -> id)

    let loadSubscriptions (pid, u) =
        parsers
        |> List.find (fun (id, _, _) -> id = pid)
        |> fun (_, ls, _) -> ls u |> Async.catch

    SU.main parserIds loadSubscriptions

let workerMainSnap parsers =
    let loadSnapshots (pid, u) =
        parsers
        |> List.find (fun (id, _, _) -> id = pid)
        |> fun (_, _, ls) -> ls u |> Async.catch

    SN.main loadSnapshots

module TelegramEventAdapter =
    let handleCommand sendToTelegram (cmd: Command) =
        match cmd with
        | :? SendTelegramMessage as SendTelegramMessage (user, msg) ->
            sendToTelegram user msg
            |> Async.Ignore
            |> Async.Start
        | _ -> ()

    let generateEvents readFromTelegram (dispatch: Event -> unit) =
        async {
            while true do
                let! (user: string, msg: string) = readFromTelegram
                dispatch (TelegramMessageReceived(user, msg))
        }

module TimerAdapter =
    let generateEvents (dispatch: Event -> unit) =
        async {
            let mutable i = 0L

            while true do
                dispatch (TimerTicked i)
                i <- i + 1L
                do! Async.Sleep 60_000
        }

let mkApplication
    sendToTelegram
    readFromTelegram
    downloadString
    isProduction
    syncSnapPeriod
    (connectionString: string)
    =
    async {
        let parsers = [ Worker.RssParser.create downloadString ]
        let store: Event EventPersistent.Store.t = TP.init ()
        let botState = StoreAtom.make ()
        let notifState = StoreAtom.make ()
        let persCache = AsyncChannel.make ()

        let handleEvent e =
            [ yield! (StoreAtom.addStateCofx botState Bot.handleEvent) e
              yield! (StoreAtom.addStateCofx notifState Notifications.handleEvent) e ]

        let handleCommand cmd =
            StoreAtom.handleCommand botState cmd
            TelegramEventAdapter.handleCommand sendToTelegram cmd
            StoreAtom.handleCommand notifState cmd

        let db = DatabaseAdapter.make connectionString
        do! Persistent.restore db (TP.make store () (fun _ _ -> ()))

        let healthState = HealthCheck.init ()

        let (handleEvent, handleCommand, productionTasks) =
            if isProduction then
                let handleEvent e =
                    [ yield! handleEvent e
                      yield! HealthCheck.handleEvent e
                      yield! Logger.logEvent e ]

                let handleCommand cmd =
                    handleCommand cmd
                    HealthCheck.handleCmd healthState cmd
                    Logger.logCommand cmd

                handleEvent, handleCommand, [ HealthCheck.main healthState ]
            else
                handleEvent, ignore, []

        let dispatch = TP.makeDispatch store handleEvent handleCommand

        printfn "Started..."

        do!
            [ yield TimerAdapter.generateEvents dispatch
              yield TelegramEventAdapter.generateEvents readFromTelegram dispatch
              yield
                  Async.delayAfter
                      (TimeSpan.FromSeconds 2.0)
                      (workerMainSub parsers (TP.make store SU.State.Empty SU.restore))
              yield Async.delayAfter syncSnapPeriod (workerMainSnap parsers (TP.make store SN.State.Empty SN.restore))
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
        Worker.RssParser.Http.download
        true
        (TimeSpan.FromMinutes(float config.updateTimeMinutes))
        (IO.Path.Combine(config.filesDir, "spectator.db"))
    |> Async.RunSynchronously

    0
