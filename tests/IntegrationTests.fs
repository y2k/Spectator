module IntegrationTests

open Xunit
module T = TestFramework

[<Fact>]
let ``save and restore state`` () =
    T.startApplication ()
    |> T.executeCommand_ "/ls" "Your subscriptions:"
    |> T.executeCommandOnce_ "/add https://degoes.net/feed.xml" "Your subscription created"
    |> T.executeCommand_ "/ls" "Your subscriptions:\n- [RSS] https://degoes.net/feed.xml '' (18)"
    |> T.resetApplication
    |> T.executeCommand_ "/ls" "Your subscriptions:\n- [RSS] https://degoes.net/feed.xml '' (18)"

[<Fact>]
let ``simple rss test`` () =
    let env = T.startApplication ()
    T.executeCommand env "/ls" "Your subscriptions:"
    T.executeCommandOnce env "/add https://degoes.net/feed.xml" "Your subscription created"
    T.executeCommand env "/ls" "Your subscriptions:\n- [RSS] https://degoes.net/feed.xml '' (18)"
    T.setDownloadStage env 1

    T.waitForMessage env "A Brief History of ZIO\n\n<a href=\"https://degoes.net/articles/zio-history\">[ OPEN ]</a>"

    T.executeCommand env "/ls" "Your subscriptions:\n- [RSS] https://degoes.net/feed.xml '' (19)"
    T.setDownloadStage env 2

    T.waitForMessage
        env
        "Effect Tracking Is Commercially Worthless\n\n<a href=\"https://degoes.net/articles/no-effect-tracking\">[ OPEN ]</a>"

    T.executeCommand env "/ls" "Your subscriptions:\n- [RSS] https://degoes.net/feed.xml '' (20)"
    T.executeCommandOnce env "/rm https://degoes.net/feed.xml" "Your subscription deleted"
    T.executeCommand env "/ls" "Your subscriptions:"

[<Fact>]
let ``simple jr test`` () =
    let env = T.startApplication ()
    T.executeCommandOnce env "/add http://joyreactor.cc/rss/tag/котэ" "Your subscription created"
    T.executeCommand env "/ls" "Your subscriptions:\n- [RSS] http://joyreactor.cc/rss/tag/котэ '' (10)"

    T.executeCommand
        env
        "/history http://joyreactor.cc/rss/tag/котэ"
        "History:\n- http://joyreactor.cc/post/4495852\n- http://joyreactor.cc/post/4495800\n- http://joyreactor.cc/post/4495765\n- http://joyreactor.cc/post/4495758\n- http://joyreactor.cc/post/4495699\n- http://joyreactor.cc/post/4495573\n- http://joyreactor.cc/post/4495568\n- http://joyreactor.cc/post/4495511\n- http://joyreactor.cc/post/4495353\n- http://joyreactor.cc/post/4494565"

[<Fact>]
let ``add and remove other test`` () =
    let env = T.startApplication ()
    T.executeCommandOnce env "/add https://degoes.net/feed.xml" "Your subscription created"
    T.executeCommandOnce env "/add http://joyreactor.cc/rss/tag/котэ" "Your subscription created"

    T.executeCommand
        env
        "/ls"
        "Your subscriptions:\n- [RSS] http://joyreactor.cc/rss/tag/котэ '' (10)\n- [RSS] https://degoes.net/feed.xml '' (18)"

    T.executeCommandOnce env "/rm https://degoes.net/feed.xml" "Your subscription deleted"
    T.executeCommand env "/ls" "Your subscriptions:\n- [RSS] http://joyreactor.cc/rss/tag/котэ '' (10)"
