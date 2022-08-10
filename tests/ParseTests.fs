module Tests

open System
open Xunit
open Spectator.Core
open Swensen.Unquote

module P = Spectator.Worker.RssParser

module private Infrastructure =
    open System.IO
    open System.Xml.Linq

    let loadFromDisk = sprintf "../../../examples/%s" >> File.ReadAllText
    let loadDocFromDisk name = name |> loadFromDisk |> XDocument.Parse

[<Fact>]
let ``degoes xml`` () =
    let xml = Infrastructure.loadFromDisk "degoes.xml"
    let actual = P.Parser.isValid xml
    test <@ actual @>
    let actual = P.Parser.getNodes xml |> List.length
    test <@ 20 = actual @>

[<Fact>]
let ``parsing wikipedia atom is success`` () =
    let xs =
        Infrastructure.loadFromDisk "wiki_atom.xml"
        |> P.Parser.getNodes

    Assert.Equal(50, xs.Length)

[<Fact>]
let ``parse simple rss`` () =
    let xs =
        Infrastructure.loadFromDisk "rss1.xml"
        |> P.Parser.getNodes

    Assert.Equal(
        [ { subscriptionId = TypedId.empty ()
            created = DateTime.Parse "2003-06-03T09:39:21Z"
            id = TypedId.empty ()
            title = "Star City"
            uri = Uri "http://liftoff.msfc.nasa.gov/news/2003/news-starcity.asp" }
          { subscriptionId = TypedId.empty ()
            created = DateTime.Parse "2003-05-30T11:06:42Z"
            id = TypedId.empty ()
            title = "Space Exploration"
            uri = Uri "http://liftoff.msfc.nasa.gov/" }
          { subscriptionId = TypedId.empty ()
            created = DateTime.Parse "2003-05-27T08:37:32Z"
            id = TypedId.empty ()
            title = "The Engine That Does More"
            uri = Uri "http://liftoff.msfc.nasa.gov/news/2003/news-VASIMR.asp" }
          { subscriptionId = TypedId.empty ()
            created = DateTime.Parse "2003-05-20T08:56:02Z"
            id = TypedId.empty ()
            title = "Astronauts' Dirty Laundry"
            uri = Uri "http://liftoff.msfc.nasa.gov/news/2003/news-laundry.asp" } ]
        |> box,
        xs |> box
    )

[<Fact>]
let ``parse simple atom`` () =
    let xs =
        Infrastructure.loadFromDisk "atom1.xml"
        |> P.Parser.getNodes

    Assert.Equal(
        [ { subscriptionId = TypedId.empty ()
            created = DateTime.Parse "2003-12-13T18:30:02Z"
            id = TypedId.empty ()
            title = "Фотографии из Африки"
            uri = Uri("http://example.org/2003/12/13/atom03") } ]
        |> box,
        xs |> box
    )
