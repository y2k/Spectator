module Spectator.Tests.WorkerTests

module D = Spectator.Worker.App.Domain

open System
open Xunit
open Swensen.Unquote
open Spectator.Core

module Utils =
    let db =
        { CoEffectDb.empty with
              newSubscriptions =
                  [ { id = Guid.NewGuid()
                      userId = "user"
                      uri = Uri "https://google.com/"
                      filter = "" }
                    { id = Guid.NewGuid()
                      userId = "user"
                      uri = Uri "https://wikipedia.org/rss.xml"
                      filter = "" } ] }
    let plugins = List.init 3 ^ fun _ -> Guid.NewGuid()

[<Fact>]
let ``mkNewSnapshots test``() =
    let db =
        D.mkNewSubscriptions.before Utils.db Utils.plugins
        |> List.map ^ fun x -> x, true
        |> D.mkNewSubscriptions.after Utils.db
        |> fun db -> { db with subscriptions = db.subscriptions |> List.map ^ fun x -> { x with id = Guid.NewGuid(); provider = Guid.NewGuid() } }
    test <@ (db.subscriptions |> List.groupBy (fun x -> x.provider)).Length = db.subscriptions.Length && 2 = db.subscriptions.Length @>

    let requests = D.mkNewSnapshots.before db Utils.plugins
    test <@ (requests |> List.groupBy (fun (x, _) -> x)).Length = requests.Length @>
    let expected = db.subscriptions |> List.map ^ fun x -> x.provider, x.uri
    test <@ expected = requests && 0 <> requests.Length @>

    let actual = requests |> List.map (fun r -> r, []) |> D.mkNewSnapshots.after db
    test <@ db = actual @>

    let actual =
        requests
        |> List.map ^ fun (p, u) -> (p, u), [ { subscriptionId = p; id = "0"; title = "title"; uri = u } ]
        |> D.mkNewSnapshots.after db
    test <@ requests.Length = actual.snapshots.unwrap.Length @>
    let expected = List.map fst requests |> List.map (fun pid -> (db.subscriptions |> List.find (fun s -> s.provider = pid)).id)
    test <@ expected = (actual.snapshots.unwrap |> List.map ^ fun x -> x.subscriptionId) @>

[<Fact>]
let ``mkNewSubscriptions test``() =
    let requests = D.mkNewSubscriptions.before Utils.db Utils.plugins
    let expected = Utils.db.newSubscriptions |> List.map (fun x -> x.uri) |> List.allPairs Utils.plugins
    test <@ expected = requests @>

    let actual =
        requests
        |> List.map ^ fun x -> x, false
        |> D.mkNewSubscriptions.after Utils.db
    test <@ CoEffectDb.empty = actual @>

    let actual =
        requests
        |> List.map ^ fun x -> x, true
        |> D.mkNewSubscriptions.after Utils.db
    test <@ 2 = actual.subscriptions.Length && [] = actual.newSubscriptions @>
