module Spectator.Pushes

open MongoDB
open MongoDB.Driver
open Spectator.Core
open Telegram.Bot
open Telegram.Bot.Types
module R = Spectator.Core.MongoCollections

let private mkMessage snaps : string = failwith "???"

let private sendMessage bot id message : exn option Async = failwith "???"

let main (db : CoEffectDb) (snaps : Snapshot list) =
    async {
        let bot = Bot.makeClient()

        let! results =
            db.subscriptions
            |> List.map ^ fun x -> {| sub = x; snaps = snaps |> List.filter ^ fun i -> i.subscriptionId = x.id |}
            |> List.filter ^ fun x -> x.snaps |> (List.isEmpty >> not)
            |> List.map ^ fun x -> sendMessage bot (x.sub.userId) (mkMessage x.snaps)
            |> Async.Parallel

        let xs =
            results
            |> Array.choose id

        return ()
    }
