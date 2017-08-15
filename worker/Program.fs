open System
open EasyNetQ
open Spectator.Core
open Spectator.Worker

module I = Infrastructure

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
        let! resp = bus.RequestAsync<Command, Responses> GetNewSubscriptions 
                    |> Async.AwaitTask
        let newSubs = convertResponseToSubs resp
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
        let! resp = bus.RequestAsync<Command, Responses> GetSubscriptions 
                    |> Async.AwaitTask
        let subs = convertResponseToSnapshots resp
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