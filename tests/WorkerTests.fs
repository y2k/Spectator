module Spectator.Tests.WorkerTests

module D = Spectator.Worker.App.Domain

open System
open Xunit
open Swensen.Unquote
open Spectator.Core

[<Fact>]
let ``mkNewSubscriptions test``() =
    let db =
        { CoEffectDb.empty with
              newSubscriptions =
                  [ { id = Guid.NewGuid()
                      userId = "user"
                      uri = Uri "https://google.com/"
                      filter = "" }
                    { id = Guid.NewGuid()
                      userId = "user"
                      uri = Uri "https://wikipedia.org/rss.xml"
                      filter = "" } ] }
    let plugins = List.init 3 ^ fun _ -> Guid.NewGuid()
    let requests = D.mkNewSubscriptions.before db plugins
    
    let expected = db.newSubscriptions |> List.map (fun x -> x.uri) |> List.allPairs plugins
    test <@ expected = requests @>

    let actual = 
        requests 
        |> List.map ^ fun x -> x, false
        |> D.mkNewSubscriptions.after db

    test <@ CoEffectDb.empty = actual @>
