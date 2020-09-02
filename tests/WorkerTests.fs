module Spectator.WorkTests

// open System
// open Swensen.Unquote
// open Spectator.Core
// module W = Worker.App.StateMachine

// [<Xunit.Fact>]
// let test () =
//     let mkSnapshot title now = { Snapshot.empty with created = now; title = title }
//     let now = DateTime.Now
//     let state = { fst W.init with subscriptions = [ { Subscription.empty with filter = "xxx" } ] }
//     let update = W.update TimeSpan.Zero [ Guid.Empty ]

//     let (state, _) = update W.MkNewSnapshots state

//     let msg = W.MkNewSnapshotsEnd [ ((Guid.Empty, null), Ok [ mkSnapshot "" now ]) ]
//     let (state, eff) = update msg state
//     test <@ 1 = Map.count state.lastUpdated @>
//     test <@ 1 = List.length eff @>

//     let msg = W.MkNewSnapshotsEnd [ ((Guid.Empty, null), Ok [ mkSnapshot "xxx" (now + TimeSpan.FromSeconds 1.) ]) ]
//     let (state, eff) = update msg state
//     test <@ 2 = List.length eff @>
