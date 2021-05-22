module Spectator.Application

open System
open Spectator
open Spectator.Core

module StoreWrapper =
    module S = EventPersistent.Store
    let init = S.init

    let make store initState update =
        let reduce = S.make store initState update

        { new IReducer<'state, Events> with
            member _.Invoke f =
                async {
                    let! oldState =
                        reduce
                            (fun state ->
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
module M = Store.MongoDb
module H = HealthCheck

let workerMainSub parsers =
    let parserIds =
        parsers |> List.map (fun (id, _, _) -> id)

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

let mkApplication sendToTelegram readFromTelegram downloadString enableLogs insert delete queryAll syncSnapPeriod =
    (* Worker.TelegramParser.create config.restTelegramPassword config.restTelegramBaseUrl *)
    (* Worker.HtmlProvider.create config.filesDir *)

    let parsers =
        [ (Worker.RssParser.create downloadString) ]

    async {
        printfn "Restore state..."

        let store = TP.init ()

        let tasks =
            [ Async.andWait (TimeSpan.FromSeconds 2.0) (workerMainSub parsers (TP.make store SU.State.Empty SU.restore))
              Async.andWait syncSnapPeriod (workerMainSnap parsers (TP.make store SN.State.Empty SN.restore))
              B.main readFromTelegram sendToTelegram (TP.make store B.State.Empty B.restore)
              Async.andWait
                  (TimeSpan.FromSeconds 2.0)
                  (N.main sendToTelegram (TP.make store N.State.Empty N.State.update)) ]

        do! P.restore queryAll (TP.make store () (fun _ _ -> ()))

        let logTasks =
            if enableLogs then
                Logger.log (TP.make store)
                [ H.mainWithDeps (TP.make store) ]
            else
                []

        printfn "Started..."

        do!
            Async.loopAll [ yield! logTasks
                            yield! tasks
                            yield
                                P.main insert delete (TP.make store P.State.Empty P.update)
                                |> Async.andWait (TimeSpan.FromSeconds 2.0) ]
    }

[<EntryPoint>]
let main args =
    let config = Config.readConfig args.[0]

    let db =
        Store.MongoDb.make config.mongoDomain "spectator"

    mkApplication
        (Telegram.sendToTelegramSingle config.telegramToken)
        (Telegram.readMessage config.telegramToken)
        Worker.RssParser.Http.download
        true
        (M.insert db)
        (M.delete db)
        (M.queryAll db)
        (TimeSpan.FromMinutes(float config.updateTimeMinutes))
    |> Async.RunSynchronously

    0
