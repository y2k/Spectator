module TestsIntegration

open System
open Swensen.Unquote
open Spectator.Core

module Framework =
    let assertBot repeat (input : Threading.Channels.Channel<string>) (output : Threading.Channels.Channel<string>) msg expected =
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

        let prev = ref "<not set>"
        let read () =
            let (success, v) = output.Reader.TryRead()
            if success
                then
                    prev := v
                    v
                else !prev

        if not repeat && not <| String.IsNullOrEmpty msg then
            input.Writer.WriteAsync msg |> ignore
        flaky 10 (async {
            if repeat && not <| String.IsNullOrEmpty msg then
                input.Writer.WriteAsync msg |> ignore
            let actual = read ()
            // printfn "\nLOG :: %O\n" actual
            test <@ expected = actual @>
        })

    let mkDownloadString stage url =
        IO.Path.Combine(
            IO.Directory.GetCurrentDirectory(),
            sprintf "../../../../tests/examples/%i/" stage,
            Uri.EscapeDataString (string url))
        |> IO.File.ReadAllText
        |> async.Return

    let run f =
        let output = Threading.Channels.Channel.CreateUnbounded<string> ()
        let input = Threading.Channels.Channel.CreateUnbounded<string> ()
        let assertBotNone = assertBot false input output ""
        let assertBotOnce = assertBot false input output
        let assertBot = assertBot true input output
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
                (Spectator.Store.MongoDb.Fake.make ())
                (fun url -> !downloadString url)
                false
        async {
            let! _ = app |> Async.StartChild
            do! f assertBot assertBotOnce assertBotNone downloadString
        } |> Async.RunSynchronously

[<Xunit.Fact>]
let ``simple rss test`` () =
    Framework.run @@ fun assertBot assertBotOnce assertBotNone downloadString -> async {
        do! assertBot "/ls" "Your subscriptions:"
        do! assertBotOnce "/add https://degoes.net/feed.xml" "Your subscription created"
        do! assertBot "/ls" "Your subscriptions:\n- [RSS] https://degoes.net/feed.xml '' (18)"

        downloadString := Framework.mkDownloadString 1
        do! assertBotNone "A Brief History of ZIO\n\n<a href=\"https://degoes.net/articles/zio-history\">[ OPEN ]</a>"
        do! assertBot "/ls" "Your subscriptions:\n- [RSS] https://degoes.net/feed.xml '' (19)"

        downloadString := Framework.mkDownloadString 2
        do! assertBotNone "Effect Tracking Is Commercially Worthless\n\n<a href=\"https://degoes.net/articles/no-effect-tracking\">[ OPEN ]</a>"
        do! assertBot "/ls" "Your subscriptions:\n- [RSS] https://degoes.net/feed.xml '' (20)"

        do! assertBotOnce "/rm https://degoes.net/feed.xml" "Your subscription deleted"
        do! assertBot "/ls" "Your subscriptions:"
    }

[<Xunit.Fact>]
let ``simple jr test`` () =
    Framework.run @@ fun assertBot assertOnce _ _ -> async {
        do! assertOnce "/add http://joyreactor.cc/rss/tag/котэ" "Your subscription created"
        do! assertBot "/ls" "Your subscriptions:\n- [RSS] http://joyreactor.cc/rss/tag/котэ '' (10)"
        do! assertBot "/history http://joyreactor.cc/rss/tag/котэ" "History:\n- http://joyreactor.cc/post/4495852\n- http://joyreactor.cc/post/4495800\n- http://joyreactor.cc/post/4495765\n- http://joyreactor.cc/post/4495758\n- http://joyreactor.cc/post/4495699\n- http://joyreactor.cc/post/4495573\n- http://joyreactor.cc/post/4495568\n- http://joyreactor.cc/post/4495511\n- http://joyreactor.cc/post/4495353\n- http://joyreactor.cc/post/4494565"
    }

[<Xunit.Fact>]
let ``add and remove other test`` () =
    Framework.run @@ fun assertBot assertOnce _ _ -> async {
        do! assertOnce "/add https://degoes.net/feed.xml" "Your subscription created"
        do! assertOnce "/add http://joyreactor.cc/rss/tag/котэ" "Your subscription created"
        do! assertBot "/ls" "Your subscriptions:\n- [RSS] http://joyreactor.cc/rss/tag/котэ '' (10)\n- [RSS] https://degoes.net/feed.xml '' (18)"

        do! assertOnce "/rm https://degoes.net/feed.xml" "Your subscription deleted"
        do! assertBot "/ls" "Your subscriptions:\n- [RSS] http://joyreactor.cc/rss/tag/котэ '' (10)"
    }
