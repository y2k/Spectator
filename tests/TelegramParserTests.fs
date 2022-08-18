module TelegramParserTests

open Swensen.Unquote
open Xunit
open System
open Spectator.Core
open Spectator.Worker.TelegramWorker

[<Fact>]
let test () =
    let state =
        { Snapshots.State.empty with
            subscriptions = [ { Subscription.empty with uri = Uri "https://t.me/reasonml_ru/22645" } ] }

    let actual = Snapshots.handleEvent state (TimerTicked 0)

    test <@ [] = actual @>

// [<Fact>]
// let test2 () =
//     let state =
//         { Subscriptions.State.empty with
//             newSubs = [ { NewSubscription.empty with uri = Uri "https://t.me/reasonml_ru/22645" } ] }

//     let actual = Subscriptions.handleEvent state (TimerTicked 0)

//     test <@ [] = actual @>
