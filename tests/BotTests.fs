module Spectator.BotTests

open System
open Xunit
open Swensen.Unquote
module B = Bot.App.Domain
module C = Core
type DB = Core.CoEffectDb

[<Fact>]
let ``test /ls for empty`` () =
    let (db2, (B.Message msg)) = B.handle { text = "/ls"; user = "" } DB.empty
    test <@ db2 = DB.empty @>
    test <@ "Your subscriptions: " = msg @>

[<Fact>]
let ``test /ls`` () =
    let s1 = { C.Subscription.empty with userId = "111"; id = C.SubscriptionId (Guid.NewGuid()); uri = Uri "http://s1.com/" }
    let s2 = { C.Subscription.empty with userId = "222"; id = C.SubscriptionId (Guid.NewGuid()); uri = Uri "http://s2.com/"; filter = "xxx" }
    let db ={ DB.empty with subscriptions = [ s1; s2 ] }

    let (db2, (B.Message msg)) = B.handle { text = "/ls"; user = "000" } db
    test <@ db2 = db @>
    test <@ "Your subscriptions: " = msg @>

    let (db2, (B.Message msg)) = B.handle { text = "/ls"; user = "111" } db
    test <@ db2 = db @>
    test <@ "Your subscriptions: \n- http://s1.com/ ()" = msg @>

    let (db2, (B.Message msg)) = B.handle { text = "/ls"; user = "222" } db
    test <@ db2 = db @>
    test <@ "Your subscriptions: \n- http://s2.com/ (xxx)" = msg @>
