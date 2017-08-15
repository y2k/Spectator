open System
open EasyNetQ
open Spectator.Core
open Spectator.Worker

module I = Infrastructure

module Async = 
    let map f xa = async { let! x = xa
                           return f x }
    
    let bindAll (f : 'a -> Async<'b>) (xsa : Async<'a list>) : Async<'b list> = 
        async { 
            let! xs = xsa
            return! xs
                    |> List.map f
                    |> Async.Parallel
                    |> map Array.toList
        }

let convertResponseToNewSubscriptions = 
    function 
    | NewSubscriptions x -> x
    | _ -> []

let subWithFlag (x : NewSubscription) = 
    RssParser.isValid x.uri |> Async.map (fun f -> x.uri, f)

let uriWithFlagsToCommand xs = 
    xs
    |> List.map (fun (x, f) -> 
           match f with
           | true -> x, Provider.Rss
           | false -> x, Provider.Invalid)
    |> CreateSubscriptions

let createNewSubscriptions (bus : IBus) = 
    I.request bus GetNewSubscriptions
    |> Async.map convertResponseToNewSubscriptions
    |> Async.bindAll subWithFlag
    |> Async.map uriWithFlagsToCommand
    |> bus.PublishAsync
    |> Async.AwaitTask

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