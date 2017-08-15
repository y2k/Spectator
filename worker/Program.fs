open System
open EasyNetQ
open Spectator.Core
open Spectator.Worker

module I = Infrastructure

module Async = 
    let map f xa = async { let! x = xa
                           return f x }

let tryCreateRssSubscriptions (newSubs : NewSubscription list) xs = 
    newSubs
    |> List.zip (xs |> Array.toList)
    |> List.map (fun (isRss, x) -> 
           match isRss with
           | true -> (x.uri, Provider.Rss)
           | false -> (x.uri, Provider.Invalid))

let convertResponseToSubs = 
    function 
    | NewSubscriptions x -> x
    | _ -> []

let createNewSubscriptions (bus : IBus) = 
    async { 
        let! newSubs = bus.RequestAsync<Command, Responses> GetNewSubscriptions
                       |> Async.AwaitTask
                       |> Async.map convertResponseToSubs
        let! xs = newSubs
                  |> List.map (fun x -> RssParser.isValid x.uri)
                  |> Async.Parallel
        do! tryCreateRssSubscriptions newSubs xs
            |> CreateSubscriptions
            |> bus.PublishAsync
            |> Async.AwaitTask
    }

let convertResponseToSnapshots = 
    function 
    | Subscriptions xs -> xs
    | _ -> []

let loadNewSnapshot (bus : IBus) = 
    async { 
        let! subs = bus.RequestAsync<Command, Responses> GetSubscriptions
                    |> Async.AwaitTask
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