module TestsIntegration

open System
open Swensen.Unquote
open Spectator.Core

let assertBot (input : Threading.Channels.Channel<string>) (output : Threading.Channels.Channel<string>) msg expected =
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

    flaky 20 (async {
        if not <| String.IsNullOrEmpty msg then
            input.Writer.WriteAsync msg |> ignore
        let! actual = output.Reader.ReadAsync().AsTask() |> Async.AwaitTask
        test <@ expected = actual @>
    })

let mkDownloadString stage url =
    IO.Path.Combine(
        IO.Directory.GetCurrentDirectory(),
        sprintf "../../../../tests/examples/%i/" stage,
        Uri.EscapeDataString (string url))
    |> IO.File.ReadAllText
    |> async.Return

[<Xunit.Fact>]
let test () =
    let output = Threading.Channels.Channel.CreateUnbounded<string> ()
    let input = Threading.Channels.Channel.CreateUnbounded<string> ()
    let assertBot = assertBot input output
    let downloadString = ref <| mkDownloadString 0

    let app =
        Spectator.Application.mkApplication
            (fun userId message ->
                output.Writer.WriteAsync message |> ignore
                async.Return ())
            (async {
                let! m = input.Reader.ReadAsync()
                return "0", m
            })
            (fun _ -> async.Zero ())
            (fun s _ -> async.Return s)
            (fun url -> !downloadString url)

    async {
        let! _ = app |> Async.StartChild

        do! assertBot "/ls" "Your subscriptions: "
        do! assertBot "/add https://degoes.net/feed.xml" "Your subscription created"
        do! assertBot "/ls" "Your subscriptions: \n- [Processing...] https://degoes.net/feed.xml ''"
        do! assertBot "/ls" "Your subscriptions: \n- [RSS] https://degoes.net/feed.xml '' (0)"

        downloadString := mkDownloadString 1

        do! assertBot "" "Effect Tracking Is Commercially Worthless\n\n<a href=\"https://degoes.net/articles/no-effect-tracking\">[ OPEN ]</a>"

        do! assertBot "/ls" "Your subscriptions: \n- [RSS] https://degoes.net/feed.xml '' (1)"
        do! assertBot "/rm https://degoes.net/feed.xml" "Your subscription deleted"
        do! assertBot "/ls" "Your subscriptions: "
    } |> Async.RunSynchronously
