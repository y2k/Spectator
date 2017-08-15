open System
open EasyNetQ
open Spectator.Core
open Spectator.Worker

module I = Infrastructure

module Async = 
    let map f xa = async { let! x = xa
                           return f x }
    let bind f xa = async { let! x = xa
                            return! f x }
    
    let bindAll (f : 'a -> Async<'b>) (xsa : Async<'a list>) : Async<'b list> = 
        async { 
            let! xs = xsa
            return! xs
                    |> List.map f
                    |> Async.Parallel
                    |> map Array.toList
        }

module Domain = 
    let convertResponseToNewSubscriptions = 
        function 
        | NewSubscriptions x -> x
        | _ -> []
    
    let uriWithFlagsToCommand xs = 
        xs
        |> List.map (fun (x, f) -> 
               match f with
               | true -> x, Provider.Rss
               | false -> x, Provider.Invalid)
        |> CreateSubscriptions
    
    let convertResponseToRssSubscriptions r = 
        match r with
        | Subscriptions xs -> xs
        | _ -> []
        |> List.filter (fun x -> x.provider = Provider.Rss)
    
    let snapshotsToCommands rssList = 
        rssList |> List.map AddSnapshotsForSubscription

module Operations = 
    let private subWithFlag (x : NewSubscription) = 
        RssParser.isValid x.uri |> Async.map (fun f -> x.uri, f)
    
    let createNewSubscriptions (bus : IBus) = 
        I.request bus GetNewSubscriptions
        |> Async.map Domain.convertResponseToNewSubscriptions
        |> Async.bindAll subWithFlag
        |> Async.map Domain.uriWithFlagsToCommand
        |> Async.bind (I.publish bus)
    
    let private getNodesWithSubscription (x : Subscription) = 
        RssParser.getNodes x.uri |> Async.map (fun snaps -> snaps, x)
    
    let loadNewSnapshot (bus : IBus) = 
        I.request bus GetSubscriptions
        |> Async.map Domain.convertResponseToRssSubscriptions
        |> Async.bindAll getNodesWithSubscription
        |> Async.map Domain.snapshotsToCommands
        |> Async.bindAll (I.publish bus)
        |> Async.Ignore

[<EntryPoint>]
let main argv = 
    let bus = RabbitHutch.CreateBus("host=localhost")
    I.executeInLoop 10000 (fun _ -> 
        async { 
            do! Operations.createNewSubscriptions bus
            do! Operations.loadNewSnapshot bus
        })
    0