module Tests

open System
open Xunit
open Spectator.Core

module P = Spectator.Worker.RssParser
module I = Infrastructure

[<Fact>]
let ``parsing wikipedia atom is success``() = 
    let xs = I.loadDocFromDisk "wiki_atom.xml" |> P.parseDocument
    Assert.Equal(50, xs.Length)
    
[<Fact>]
let ``parse simple rss``() = 
    let xs = I.loadDocFromDisk "rss1.xml" |> P.parseDocument
    Assert.Equal(xs |> box, 
                 [ { subscriptionId = Guid.Empty
                     title = "Star City"
                     uri = Uri "http://liftoff.msfc.nasa.gov/news/2003/news-starcity.asp" }
                   { subscriptionId = Guid.Empty
                     title = "Space Exploration"
                     uri = Uri "http://liftoff.msfc.nasa.gov/" }
                   { subscriptionId = Guid.Empty
                     title = "The Engine That Does More"
                     uri = Uri "http://liftoff.msfc.nasa.gov/news/2003/news-VASIMR.asp" }
                   { subscriptionId = Guid.Empty
                     title = "Astronauts' Dirty Laundry"
                     uri = Uri "http://liftoff.msfc.nasa.gov/news/2003/news-laundry.asp" } ]
                 |> box)

[<Fact>]
let ``parse simple atom``() = 
    let xs = I.loadDocFromDisk "atom1.xml" |> P.parseDocument
    Assert.Equal(xs |> box, 
                 [ { subscriptionId = Guid.Empty
                     title = "Фотографии из Африки"
                     uri = Uri("http://example.org/2003/12/13/atom03") } ]
                 |> box)