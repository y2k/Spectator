﻿open System
open EasyNetQ
open Spectator.Core
open Spectator.Worker

[<EntryPoint>]
let main argv =
    // let doc = System.Xml.Linq.XDocument.Parse(System.IO.File.ReadAllText("examples/wiki_atom.xml"))
    // let result = RssParser.parseDocument doc
    // printfn "result = %A" result

    let bus = RabbitHutch.CreateBus("host=localhost")
    let rssProvider = RssNodeProvider() :> INodeProvider
    let rec doWork () = async {
        let! resp = bus.RequestAsync<Command, Responses> GetNewSubscriptions |> Async.AwaitTask
        let newSubs = match resp with | NewSubscriptions x -> x | _ -> []
        let! xs = newSubs
                  |> List.map (fun x -> rssProvider.IsValid x.uri)
                  |> Async.Parallel

        let subs = newSubs |> List.zip (xs |> Array.toList)
                           |> List.map (fun (isRss, x) -> match isRss with
                                                          | true  -> (x.uri, Provider.Rss)
                                                          | false -> (x.uri, Provider.Invalid))

        do! CreateSubscriptions subs |> bus.PublishAsync |> Async.AwaitTask

        do! Async.Sleep(10000)
        return! doWork ()
    }

    doWork () |> ignore
    
    System.Threading.Thread.Sleep(-1)
    0