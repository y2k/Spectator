module Spectator.Logger

module TP = EventPersistent.Tea

let log store =
    TP.make store () (fun _ e -> printfn "LOG event ::\n%O" e)
    |> ignore
