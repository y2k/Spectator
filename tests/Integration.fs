module TestsIntegration

[<Xunit.Fact>]
let ``save and restore state`` () =
    TestFramework.run
    <| fun env ->
        async {
            do! env.executeCommand "/ls" "Your subscriptions:"
            do! env.executeCommandOnce "/add https://degoes.net/feed.xml" "Your subscription created"
            do! env.executeCommand "/ls" "Your subscriptions:\n- [RSS] https://degoes.net/feed.xml '' (18)"

            env.resetApplication ()

            do! env.executeCommand "/ls" "Your subscriptions:\n- [RSS] https://degoes.net/feed.xml '' (18)"
        }

[<Xunit.Fact>]
let ``simple rss test`` () =
    TestFramework.run
    <| fun env ->
        async {
            do! env.executeCommand "/ls" "Your subscriptions:"
            do! env.executeCommandOnce "/add https://degoes.net/feed.xml" "Your subscription created"
            do! env.executeCommand "/ls" "Your subscriptions:\n- [RSS] https://degoes.net/feed.xml '' (18)"

            env.setDownloadStage 1

            do! env.waitForMessage "A Brief History of ZIO\n\n<a href=\"https://degoes.net/articles/zio-history\">[ OPEN ]</a>"
            do! env.executeCommand "/ls" "Your subscriptions:\n- [RSS] https://degoes.net/feed.xml '' (19)"

            env.setDownloadStage 2

            do! env.waitForMessage "Effect Tracking Is Commercially Worthless\n\n<a href=\"https://degoes.net/articles/no-effect-tracking\">[ OPEN ]</a>"
            do! env.executeCommand "/ls" "Your subscriptions:\n- [RSS] https://degoes.net/feed.xml '' (20)"

            do! env.executeCommandOnce "/rm https://degoes.net/feed.xml" "Your subscription deleted"
            do! env.executeCommand "/ls" "Your subscriptions:"
        }

[<Xunit.Fact>]
let ``simple jr test`` () =
    TestFramework.run
    <| fun env ->
        async {
            do! env.executeCommandOnce "/add http://joyreactor.cc/rss/tag/котэ" "Your subscription created"
            do! env.executeCommand "/ls" "Your subscriptions:\n- [RSS] http://joyreactor.cc/rss/tag/котэ '' (10)"

            do! env.executeCommand "/history http://joyreactor.cc/rss/tag/котэ" "History:\n- http://joyreactor.cc/post/4495852\n- http://joyreactor.cc/post/4495800\n- http://joyreactor.cc/post/4495765\n- http://joyreactor.cc/post/4495758\n- http://joyreactor.cc/post/4495699\n- http://joyreactor.cc/post/4495573\n- http://joyreactor.cc/post/4495568\n- http://joyreactor.cc/post/4495511\n- http://joyreactor.cc/post/4495353\n- http://joyreactor.cc/post/4494565"
        }

[<Xunit.Fact>]
let ``add and remove other test`` () =
    TestFramework.run
    <| fun env ->
        async {
            do! env.executeCommandOnce "/add https://degoes.net/feed.xml" "Your subscription created"
            do! env.executeCommandOnce "/add http://joyreactor.cc/rss/tag/котэ" "Your subscription created"

            do! env.executeCommand "/ls" "Your subscriptions:\n- [RSS] http://joyreactor.cc/rss/tag/котэ '' (10)\n- [RSS] https://degoes.net/feed.xml '' (18)"

            do! env.executeCommandOnce "/rm https://degoes.net/feed.xml" "Your subscription deleted"
            do! env.executeCommand "/ls" "Your subscriptions:\n- [RSS] http://joyreactor.cc/rss/tag/котэ '' (10)"
        }
