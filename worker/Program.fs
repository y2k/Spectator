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
    open AsyncOperators

    let private subWithFlag (x : NewSubscription) = 
        RssParser.isValid x.uri 
        >>- fun isValid -> x.userId, x.uri, isValid

    let createNewSubscription bus newScription = 
        newScription
        |> subWithFlag
        |> Async.map3 Domain.uriWithFlagsToCommand
        >>= Bus.publish bus
    
    let createNewSubscriptions bus = 
        Bus.request bus GetNewSubscriptions
        >>- Domain.convertResponseToNewSubscriptions
        |> Async.bindAll (createNewSubscription bus)
        >>- ignore
    
    let private getNodesWithSubscription (x : Subscription) = 
        RssParser.getNodes x.uri 
        >>- fun snaps -> snaps, x
    
    let loadNewSnapshot bus = 
        Bus.request bus GetSubscriptions
        >>- Domain.convertResponseToRssSubscriptions
        |> Async.bindAll getNodesWithSubscription
        >>- Domain.snapshotsToCommands
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