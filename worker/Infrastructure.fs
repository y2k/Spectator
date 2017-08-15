module Infrastructure

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