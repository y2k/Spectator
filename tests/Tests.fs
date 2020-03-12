module Tests

open System
open Xunit
open Spectator.Core

module P = Spectator.Worker.RssParser
module I = Infrastructure

[<Fact>]
let ``parsing wikipedia atom is success``() =
    let xs = I.loadFromDisk "wiki_atom.xml" |> P.Parser.getNodes
    Assert.Equal(50, xs.Length)

[<Fact>]
let ``parse simple rss``() =
    let xs = I.loadFromDisk "rss1.xml" |> P.Parser.getNodes
    Assert.Equal([ { subscriptionId = TypedId.empty ()
                     id = TypedId.empty ()
                     title = "Star City"
                     uri = Uri "http://liftoff.msfc.nasa.gov/news/2003/news-starcity.asp" }
                   { subscriptionId = TypedId.empty ()
                     id = TypedId.empty ()
                     title = "Space Exploration"
                     uri = Uri "http://liftoff.msfc.nasa.gov/" }
                   { subscriptionId = TypedId.empty ()
                     id = TypedId.empty ()
                     title = "The Engine That Does More"
                     uri = Uri "http://liftoff.msfc.nasa.gov/news/2003/news-VASIMR.asp" }
                   { subscriptionId = TypedId.empty ()
                     id = TypedId.empty ()
                     title = "Astronauts' Dirty Laundry"
                     uri = Uri "http://liftoff.msfc.nasa.gov/news/2003/news-laundry.asp" } ]
                 |> box, xs |> box)

[<Fact>]
let ``parse simple atom``() =
    let xs = I.loadFromDisk "atom1.xml" |> P.Parser.getNodes
    Assert.Equal([ { subscriptionId = TypedId.empty ()
                     id = TypedId.empty ()
                     title = "Фотографии из Африки"
                     uri = Uri("http://example.org/2003/12/13/atom03") } ]
                 |> box, xs |> box)
