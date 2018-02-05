namespace Spectator

module Bus = 
    open EasyNetQ
    open Core
    
    let publish (bus : IBus) command = 
        bus.PublishAsync<Command> command |> Async.AwaitTask
    let request (bus : IBus) command = 
        bus.RequestAsync<Command, Responses> command |> Async.AwaitTask
    let TODO _ = raise (System.NotImplementedException())

module Infrastructure = 
    let executeInLoop time action = 
        let rec doWork() = 
            async { 
                do! action
                do! Async.Sleep time
                do! doWork()
            }
        doWork() |> Async.RunSynchronously