open System
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
        let! newSubs = bus.RequestAsync<Requests, SubscriptionRequest list> NewSubscriptions |> Async.AwaitTask
        let! xs = newSubs
                  |> List.map (fun x -> rssProvider.IsValid x.url)
                  |> Async.Parallel

        let! _ = newSubs |> List.zip (xs |> Array.toList)
                         |> List.map (fun (isRss, x) -> match isRss with
                                                        | true  -> { id = x.id; provider = Rss }
                                                        | false -> { id = x.id; provider = WebPage }
                                                        |> bus.PublishAsync |> Async.AwaitTask)
                         |> Async.Parallel

        do! Async.Sleep(10000)
        return! doWork ()
    }

    doWork () |> ignore
    
    System.Threading.Thread.Sleep(-1)
    0