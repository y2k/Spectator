namespace IntegrationTests

open Xunit
module T = TestFramework

module ``save and restore state`` =
    [<Fact>]
    let test () =
        let env = T.startApplication ()
        T.executeCommand env "/ls" "Your subscriptions:"
        T.executeCommandOnce env "/add https://degoes.net/feed.xml" "Your subscription created"
        T.executeCommand env "/ls" "Your subscriptions:\n- [RSS] https://degoes.net/feed.xml '' (18)"
        let env = T.resetApplication env
        T.executeCommand env "/ls" "Your subscriptions:\n- [RSS] https://degoes.net/feed.xml '' (18)"

module ``simple rss test`` =
    [<Fact>]
    let test () =
        let env = T.startApplication ()
        T.executeCommand env "/ls" "Your subscriptions:"
        T.executeCommandOnce env "/add https://degoes.net/feed.xml" "Your subscription created"
        T.executeCommand env "/ls" "Your subscriptions:\n- [RSS] https://degoes.net/feed.xml '' (18)"
        T.setDownloadStage env 1

        T.waitForMessage
            env
            "A Brief History of ZIO\n\n<a href=\"https://degoes.net/articles/zio-history\">[ OPEN ]</a>"

        T.executeCommand env "/ls" "Your subscriptions:\n- [RSS] https://degoes.net/feed.xml '' (19)"
        T.setDownloadStage env 2

        T.waitForMessage
            env
            "Effect Tracking Is Commercially Worthless\n\n<a href=\"https://degoes.net/articles/no-effect-tracking\">[ OPEN ]</a>"

        T.executeCommand env "/ls" "Your subscriptions:\n- [RSS] https://degoes.net/feed.xml '' (20)"
        T.executeCommandOnce env "/rm https://degoes.net/feed.xml" "Your subscription deleted"
        T.executeCommand env "/ls" "Your subscriptions:"

module ``simple jr test`` =
    [<Fact>]
    let test () =
        let env = T.startApplication ()
        T.executeCommandOnce env "/add http://joyreactor.cc/rss/tag/котэ" "Your subscription created"
        T.executeCommand env "/ls" "Your subscriptions:\n- [RSS] http://joyreactor.cc/rss/tag/котэ '' (10)"

        T.executeCommand
            env
            "/history http://joyreactor.cc/rss/tag/котэ"
            "History:\n- http://joyreactor.cc/post/4495852\n- http://joyreactor.cc/post/4495800\n- http://joyreactor.cc/post/4495765\n- http://joyreactor.cc/post/4495758\n- http://joyreactor.cc/post/4495699\n- http://joyreactor.cc/post/4495573\n- http://joyreactor.cc/post/4495568\n- http://joyreactor.cc/post/4495511\n- http://joyreactor.cc/post/4495353\n- http://joyreactor.cc/post/4494565"

module ``add and remove other test`` =
    [<Fact>]
    let test () =
        let env = T.startApplication ()
        T.executeCommandOnce env "/add https://degoes.net/feed.xml" "Your subscription created"
        T.executeCommandOnce env "/add http://joyreactor.cc/rss/tag/котэ" "Your subscription created"

        T.executeCommand
            env
            "/ls"
            "Your subscriptions:\n- [RSS] http://joyreactor.cc/rss/tag/котэ '' (10)\n- [RSS] https://degoes.net/feed.xml '' (18)"

        T.executeCommandOnce env "/rm https://degoes.net/feed.xml" "Your subscription deleted"
        T.executeCommand env "/ls" "Your subscriptions:\n- [RSS] http://joyreactor.cc/rss/tag/котэ '' (10)"
