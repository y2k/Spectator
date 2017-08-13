module Tests

open System
open Xunit
open Spectator.Core

module P = Spectator.Worker.RssParser
module I = Infrastructure

[<Fact>]
let ``parse simple rss``() = 
    let xs = I.loadDocFromDisk "rss1.xml" |> P.parseDocument
    Assert.Equal(4, xs.Length)

[<Fact>]
let ``parse simple atom``() = 
    let xs = I.loadDocFromDisk "atom1.xml" |> P.parseDocument
    Assert.Equal(xs |> box, 
                 [ { subscriptionId = Guid.Empty
                     title = "Фотографии из Африки"
                     uri = Uri("http://example.org/2003/12/13/atom03") } ]
                 |> box)