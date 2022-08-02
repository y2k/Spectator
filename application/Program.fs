module Spectator.Application

open System
open Spectator
open Spectator.Core

module StoreWrapper =
    open Spectator.Core

    module S = EventPersistent.Store
    let init = S.init

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

module N = Notifications
module SN = Worker.SnapshotsMain
module SU = Worker.SubscriptionsMain
module B = Bot.App
module P = Store.Persistent
module TP = StoreWrapper
module M = Store.DatabaseAdapter
module H = HealthCheck

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

let mkApplication sendToTelegram readFromTelegram downloadString enableLogs syncSnapPeriod (connectionString: string) =
    let parsers = [ (Worker.RssParser.create downloadString) ]

    let db = Store.DatabaseAdapter.make connectionString

    async {
        printfn "Restore state..."

        let store: Event EventPersistent.Store.t = TP.init ()

        let tasks =
            [ Async.andWait (TimeSpan.FromSeconds 2.0) (workerMainSub parsers (TP.make store SU.State.Empty SU.restore))
              Async.andWait syncSnapPeriod (workerMainSnap parsers (TP.make store SN.State.Empty SN.restore))
              B.main readFromTelegram sendToTelegram (TP.make store B.State.Empty B.restore)
              Async.andWait
                  (TimeSpan.FromSeconds 2.0)
                  (N.main sendToTelegram (TP.make store N.State.Empty N.State.update)) ]

        do! P.restore db (TP.make store () (fun _ _ -> ()))

        let logTasks =
            if enableLogs then
                Logger.log (TP.make store)
                [ H.main (TP.make store) ]
            else
                []

        printfn "Started..."

        do!
            Async.loopAll [ yield! logTasks
                            yield! tasks
                            yield
                                P.main db (TP.make store P.State.Empty P.update)
                                |> Async.andWait (TimeSpan.FromSeconds 2.0) ]
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
