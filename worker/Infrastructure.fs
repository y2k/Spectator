module Infrastructure

open EasyNetQ
open Spectator.Core

let publish (bus : IBus) command = bus.PublishAsync<Command> command |> Async.AwaitTask

let request (bus : IBus) command = 
    bus.RequestAsync<Command, Responses> command |> Async.AwaitTask
let TODO _ = raise (System.NotImplementedException())

let executeInLoop time action = 
    let rec doWork() = 
        async { 
            do! action()
            do! Async.Sleep time
            return! doWork()
        }
    doWork() |> ignore
    System.Threading.Thread.Sleep(-1)