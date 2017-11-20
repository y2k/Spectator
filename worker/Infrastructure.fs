namespace Spectator

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

module Bus = 
    open EasyNetQ
    open Spectator.Core
    
    let publish (bus : IBus) command = 
        bus.PublishAsync<Command> command |> Async.AwaitTask
    let request (bus : IBus) command = 
        bus.RequestAsync<Command, Responses> command |> Async.AwaitTask
    let TODO _ = raise (System.NotImplementedException())

module Infrastructure = 
    let executeInLoop time action = 
        let rec doWork() = 
            async { 
                do! action()
                do! Async.Sleep time
                return! doWork()
            }
        doWork() |> Async.RunSynchronously