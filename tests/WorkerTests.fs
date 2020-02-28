module Spectator.Tests.WorkerTests

module D = Spectator.Worker.App.Services

open System
open Xunit
open Swensen.Unquote
open Spectator.Core

module Utils =
    let db =
        { CoEffectDb.empty with
              newSubscriptions =
                  [ { id = TypedId.wrap ^ Guid.NewGuid()
                      userId = "user"
                      uri = Uri "https://google.com/"
                      filter = "" }
                    { id = TypedId.wrap ^ Guid.NewGuid()
                      userId = "user"
                      uri = Uri "https://wikipedia.org/rss.xml"
                      filter = "" } ] }
    let plugins = List.init 3 ^ fun _ -> Guid.NewGuid()

[<Fact>]
let ``make subscriptions test``() =
    let (db, (r, responseToMsg)) = 
        D.update (D.MkSubscriptions Utils.plugins) Utils.db
        |> fun (db, x) -> db, match x with [ D.LoadSubscriptions (x, f) ] -> x, f | e -> failwithf "%O" e
    test <@ Utils.plugins.Length * Utils.db.newSubscriptions.Length = r.Length @>

    let (db, _) =
        r
        |> List.map ^ fun (p, url) -> (p, url), Ok (p = Utils.plugins.[0])
        |> responseToMsg
        |> fun msg -> D.update msg db

    test <@ 0 = db.newSubscriptions.Length @>
    test <@ Utils.db.newSubscriptions.Length = db.subscriptions.Length @>
