module Spectator.BotTests

open System
open Xunit
open Swensen.Unquote
open Core

module B = Bot.App.Updater
type DB = Core.CoEffectDb

let private s1 = { Subscription.empty with userId = "111"; id = TypedId.wrap (Guid.NewGuid()); uri = Uri "http://s1.com/" }
let private ns1 = { NewSubscription.empty with userId = "111"; id = TypedId.wrap (Guid.NewGuid()); uri = Uri "http://s1-2.com/" }
let private s2 = { Subscription.empty with userId = "222"; id = TypedId.wrap (Guid.NewGuid()); uri = Uri "http://s2.com/"; filter = "xxx" }
let private db = { DB.empty with subscriptions = [ s1; s2 ]; newSubscriptions = [ ns1 ] }

[<Fact>]
let ``test /rm`` () =
    let (db2, msg) = B.handle { text = "/rm http://s1.com/"; user = "111" } db
    test <@ "Your subscription deleted" = msg @>
    test <@ 0 = (db2.subscriptions |> List.filter (fun x -> x.userId = "111") |> List.length)
         && 1 = List.length db2.newSubscriptions @>
    let (db2, msg) = B.handle { text = "/rm http://s1-2.com/"; user = "111" } db
    test <@ "Your subscription deleted" = msg @>
    test <@ 1 = (db2.subscriptions |> List.filter (fun x -> x.userId = "111") |> List.length)
         && 0 = List.length db2.newSubscriptions @>

[<Fact>]
let ``test /add`` () =
    let validate user cmd url filter  count db =
        let (db2, msg) = B.handle { text = cmd; user = user } db
        test <@ count = (db2.newSubscriptions |> List.filter (fun x -> x.userId = user) |> List.length) @>
        let s = db2.newSubscriptions |> List.find (fun x -> x.userId = user)
        test <@ filter = s.filter && Uri url = s.uri && user = s.userId @>
        test <@ "Your subscription created" = msg @>
        db2
    DB.empty
    |> validate "111" "/add http://a.com/ xxx" "http://a.com/" "xxx" 1
    |> validate "111" "/add http://b.com/" "http://b.com/" "" 2
    |> validate "222" "/add http://c.com/" "http://c.com/" "" 1

[<Fact>]
let ``test /ls`` () =
    let (db2, msg) = B.handle { text = "/ls"; user = "000" } db
    test <@ db2 = db @>
    test <@ "Your subscriptions: " = msg @>

    let (db2, msg) = B.handle { text = "/ls"; user = "111" } db
    test <@ db2 = db @>
    test <@ "Your subscriptions: \n- http://s1.com/ ()\n- (Waiting) http://s1-2.com/ ()" = msg @>

    let (db2, msg) = B.handle { text = "/ls"; user = "222" } db
    test <@ db2 = db @>
    test <@ "Your subscriptions: \n- http://s2.com/ (xxx)" = msg @>
