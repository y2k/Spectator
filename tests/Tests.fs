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
    Assert.Equal([ { subscriptionId = "http://liftoff.msfc.nasa.gov/2003/06/03.html#item573"
                     title = "Star City"
                     uri = Uri "http://liftoff.msfc.nasa.gov/news/2003/news-starcity.asp" }
                   { subscriptionId = "http://liftoff.msfc.nasa.gov/2003/05/30.html#item572"
                     title = "Space Exploration"
                     uri = Uri "http://liftoff.msfc.nasa.gov/" }
                   { subscriptionId = "http://liftoff.msfc.nasa.gov/2003/05/27.html#item571"
                     title = "The Engine That Does More"
                     uri = Uri "http://liftoff.msfc.nasa.gov/news/2003/news-VASIMR.asp" }
                   { subscriptionId = "http://liftoff.msfc.nasa.gov/2003/05/20.html#item570"
                     title = "Astronauts' Dirty Laundry"
                     uri = Uri "http://liftoff.msfc.nasa.gov/news/2003/news-laundry.asp" } ]
                 |> box,
                 xs |> box)

[<Fact>]
let ``parse simple atom``() = 
    let xs = I.loadDocFromDisk "atom1.xml" |> P.parseDocument
    Assert.Equal([ { subscriptionId = "urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a"
                     title = "Фотографии из Африки"
                     uri = Uri("http://example.org/2003/12/13/atom03") } ]
                 |> box,
                 xs |> box)