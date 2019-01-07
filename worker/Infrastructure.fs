namespace Spectator.Worker

open Spectator

module Infrastructure =
    let executeInLoop time action =
        let rec doWork() =
            async {
                do! action
                do! Async.Sleep time
                do! doWork()
            }
        doWork()
