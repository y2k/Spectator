namespace Spectator.Core

[<AutoOpen>]
module Prelude =
    module Async =
        let loopAll axs =
            let loop a =
                async {
                    while true do
                        do! a
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
