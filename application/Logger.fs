module Spectator.Logger

open Spectator.Core

let logEvent (event: Event) =
    printfn "[LOG][EVENT] ::\n%O" event
    []

let logCommand (cmd: Command) = printfn "[LOG][CMD] ::\n%O" cmd
