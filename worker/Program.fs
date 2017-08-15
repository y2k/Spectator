open System
open EasyNetQ
open Spectator.Core
open Spectator.Worker

module I = Infrastructure

module Async = 
    let map f xa = async { let! x = xa
                           return f x }

let convertResponseToSubs = 
    function 
    | NewSubscriptions x -> x
    | _ -> []

let subWithFlag (x : NewSubscription) = async { let! f = RssParser.isValid x.uri
                                                return x.uri, f }

let uriWithFlagsToCommand xs = 
    xs
    |> Array.map (fun (x, f) -> 
           match f with
           | true -> x, Provider.Rss
           | false -> x, Provider.Invalid)
    |> Array.toList
    |> CreateSubscriptions

let createNewSubscriptions (bus : IBus) = 
    async { 
        let! newSubs = I.request bus GetNewSubscriptions 
                       |> Async.map convertResponseToSubs
        do! newSubs
            |> List.map subWithFlag
            |> Async.Parallel
            |> Async.map uriWithFlagsToCommand
            |> bus.PublishAsync
            |> Async.AwaitTask
    }

let convertResponseToSnapshots = 
    function 
    | Subscriptions xs -> xs
    | _ -> []

let loadNewSnapshot (bus : IBus) = 
    async { 
        let! subs = I.request bus GetSubscriptions 
                    |> Async.map convertResponseToSnapshots
        let! rssList = subs
                       |> List.filter (fun x -> x.provider = Provider.Rss)
                       |> List.map (fun x -> async { let! snaps = RssParser.getNodes 
                                                                      x.uri
                                                     return (snaps, x) })
                       |> Async.Parallel
        do! rssList
            |> Array.map (AddSnapshotsForSubscription
                          >> bus.PublishAsync
                          >> Async.AwaitTask)
            |> Async.Parallel
            |> Async.Ignore
    }

[<EntryPoint>]
let main argv = 
    let bus = RabbitHutch.CreateBus("host=localhost")
    I.executeInLoop 10000 (fun _ -> 
        async { 
            do! createNewSubscriptions bus
            do! loadNewSnapshot bus
        })
    0