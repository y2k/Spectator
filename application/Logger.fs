module Spectator.Logger

let log makeReducer =
    makeReducer () (fun _ e -> printfn "LOG event ::\n%O" e)
    |> ignore
