open EasyNetQ
open Spectator
open Spectator.Core
open Spectator.Worker

module I = Spectator.Infrastructure

module Domain = 
    let convertResponseToNewSubscriptions = 
        function 
        | NewSubscriptions x -> x
        | _ -> []

    let uriWithFlagsToCommand userId uri isRss = 
        match isRss with
        | true -> userId, uri, Provider.Rss
        | false -> userId, uri, Provider.Invalid
        |> CreateSubscription
    
    let convertResponseToRssSubscriptions = 
        function
        | Subscriptions xs -> xs
        | _ -> []
        >> List.filter (fun x -> x.provider = Provider.Rss)

    let snapshotsToCommands = 
        List.map AddSnapshotsForSubscription

module Operations = 
    let private subWithFlag (x : NewSubscription) = 
        RssParser.isValid x.uri 
        |> Async.map (fun f -> x.userId, x.uri, f)

    let createNewSubscription (bus : IBus) newScription = 
        newScription
        |> subWithFlag
        |> Async.map3 Domain.uriWithFlagsToCommand
        |> Async.bind (Bus.publish bus)
    
    let createNewSubscriptions (bus : IBus) = 
        Bus.request bus GetNewSubscriptions
        |> Async.map Domain.convertResponseToNewSubscriptions
        |> Async.bindAll (createNewSubscription bus)
        |> Async.Ignore
    
    let private getNodesWithSubscription (x : Subscription) = 
        RssParser.getNodes x.uri |> Async.map (fun snaps -> snaps, x)
    
    let loadNewSnapshot (bus : IBus) = 
        Bus.request bus GetSubscriptions
        |> Async.map Domain.convertResponseToRssSubscriptions
        |> Async.bindAll getNodesWithSubscription
        |> Async.map Domain.snapshotsToCommands
        |> Async.bindAll (Bus.publish bus)
        |> Async.Ignore

[<EntryPoint>]
let main _ = 
    printfn "Start worker..."
    let bus = RabbitHutch.CreateBus("host=localhost;timeout=60")
    Operations.createNewSubscriptions bus
    |> Async.next (Operations.loadNewSnapshot bus)
    |> I.executeInLoop 10000
    0