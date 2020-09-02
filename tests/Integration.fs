module TestsIntegration

open System
open Swensen.Unquote

let assertBot (input : Threading.Channels.Channel<string>) (output : Threading.Channels.Channel<string>) m expected =
    let rec flaky count f  =
        async {
            try
                do! f
            with
            | e ->
                if count > 0
                    then
                        do! Async.Sleep 1_000
                        do! flaky (count - 1) f
                    else raise e
        }

    flaky 15 (async {
        input.Writer.WriteAsync m |> ignore
        let! actual = output.Reader.ReadAsync().AsTask() |> Async.AwaitTask
        test <@ expected = actual @>
    })

[<Xunit.Fact>]
let test () =
    let output = Threading.Channels.Channel.CreateUnbounded<string> ()
    let input = Threading.Channels.Channel.CreateUnbounded<string> ()
    let assertBot = assertBot input output

    let app =
        Spectator.Application.mkApplication
            (fun userId message ->
                output.Writer.WriteAsync message |> ignore
                async.Return ())
            (async {
                let! m = input.Reader.ReadAsync().AsTask() |> Async.AwaitTask
                return "0", m
            })
            (fun _ -> async.Zero ())
            (fun s _ -> async.Return s)

    async {
        let! _ = app |> Async.StartChild

        input.Writer.WriteAsync "/add https://degoes.net/feed.xml" |> ignore
        input.Writer.WriteAsync "/ls" |> ignore

        let! message = output.Reader.ReadAsync().AsTask() |> Async.AwaitTask
        test <@ "Your subscription created" = message @>
        let! message = output.Reader.ReadAsync().AsTask() |> Async.AwaitTask
        test <@ "Your subscriptions: \n- [Processing...] https://degoes.net/feed.xml ''" = message @>

        do! assertBot "/ls" "Your subscriptions: \n- [RSS] https://degoes.net/feed.xml '' (0)"
    } |> Async.RunSynchronously
