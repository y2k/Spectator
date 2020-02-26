module Spectator.BotTests

open System
open Xunit
open Swensen.Unquote
open Core

module B = Bot.App.Domain
type DB = Core.CoEffectDb

let private s1 = { Subscription.empty with userId = "111"; id = SubscriptionId (Guid.NewGuid()); uri = Uri "http://s1.com/" }
let private ns1 = { NewSubscription.empty with userId = "111"; id = SubscriptionId (Guid.NewGuid()); uri = Uri "http://s1-2.com/" }
let private s2 = { Subscription.empty with userId = "222"; id = SubscriptionId (Guid.NewGuid()); uri = Uri "http://s2.com/"; filter = "xxx" }
let private db = { DB.empty with subscriptions = [ s1; s2 ]; newSubscriptions = [ ns1 ] }

[<Fact>]
let ``test rm`` () =
    let (db2, (B.Message msg)) = B.handle { text = "/rm http://s1.com/"; user = "111" } db
    test <@ "Your subscription deleted" = msg @>
    test <@ 0 = (db2.subscriptions |> List.filter (fun x -> x.userId = "111") |> List.length)
         && 1 = List.length db2.newSubscriptions @>
    let (db2, (B.Message msg)) = B.handle { text = "/rm http://s1-2.com/"; user = "111" } db
    test <@ "Your subscription deleted" = msg @>
    test <@ 1 = (db2.subscriptions |> List.filter (fun x -> x.userId = "111") |> List.length)
         && 0 = List.length db2.newSubscriptions @>

[<Fact>]
let ``test add`` () =
    let test' cmd url filter =
        let (db2, (B.Message msg)) = B.handle { text = cmd; user = "111" } DB.empty
        test <@ 1 = List.length db2.newSubscriptions @>
        let s = db2.newSubscriptions.[0]
        test <@ filter = s.filter && Uri url = s.uri && "111" = s.userId @>
        test <@ "Your subscription created" = msg @>
    test' "/add http://g.com/ xxx" "http://g.com/" "xxx"
    test' "/add http://g.com/" "http://g.com/" ""

[<Fact>]
let ``test /ls`` () =
    let (db2, (B.Message msg)) = B.handle { text = "/ls"; user = "000" } db
    test <@ db2 = db @>
    test <@ "Your subscriptions: " = msg @>

    let (db2, (B.Message msg)) = B.handle { text = "/ls"; user = "111" } db
    test <@ db2 = db @>
    test <@ "Your subscriptions: \n- http://s1.com/ ()\n- (Waiting) http://s1-2.com/ ()" = msg @>

    let (db2, (B.Message msg)) = B.handle { text = "/ls"; user = "222" } db
    test <@ db2 = db @>
    test <@ "Your subscriptions: \n- http://s2.com/ (xxx)" = msg @>
