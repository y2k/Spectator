module BotTests

// open System
// open Swensen.Unquote
// open Spectator.Core
// module B = Spectator.Bot.App.Updater

// [<Xunit.Fact>]
// let ``bot should store last 10 snapshots`` () =
//     let msg =
//         { Subscription.empty with userId = "" }
//         |> SubscriptionCreated
//         |> B.EventsReceived
//     let (state, _) = B.update msg (fst B.init)

//     let snapshots =
//         [0 .. 50]
//         |> List.map @@ fun i -> { Snapshot.empty with uri = Uri <| sprintf "https://g.com/%i" i }
//     let state =
//         snapshots
//         |> List.map (SnapshotCreated >> B.EventsReceived)
//         |> List.fold (fun state msg -> B.update msg state |> fst) state

//     let expected = snapshots |> List.rev |> List.take 10
//     test <@ expected = state.states.[""].snapshots @>
