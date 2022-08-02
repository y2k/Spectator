namespace Spectator.Core

[<System.Obsolete>]
type IReducer<'state, 'event> =
    abstract member Invoke : ('state -> 'state * 'event list * 'result) -> 'result Async

[<AutoOpen>]
module Prelude =
    module Async =
        let andWait (timeout: System.TimeSpan) a =
            async {
                do! a
                do! Async.Sleep timeout
            }

        let loopAll axs =
            let loop a =
                async {
                    while true do
                        match! a |> Async.catch with
                        | Ok _ -> ()
                        | Error e -> eprintfn "LOG: Error: %O" e
                }

            axs
            |> List.map loop
            |> Async.Parallel
            |> Async.Ignore

    module Result =
        let toOption =
            function
            | Ok x -> Some x
            | Error _ -> None
