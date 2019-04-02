module Spectator.Worker.RssParser

open Spectator.Core
open System
open System.Xml
open System.Xml.Linq
open System.Xml.XPath

module Parser =
    let private ns = XmlNamespaceManager(NameTable())

    ns.AddNamespace("media", "http://search.yahoo.com/mrss/")
    ns.AddNamespace("atom", "http://www.w3.org/2005/Atom")

    let parseRss (doc : XDocument) =
        doc.XPathSelectElements "//channel/item"
        |> Seq.map (fun e ->
               { subscriptionId = Guid.Empty
                 id = e.XPathSelectElement("guid").Value
                 title = e.XPathSelectElement("title").Value
                 uri = e.XPathSelectElement("link").Value |> Uri })
        |> Seq.toList

    let parseAtom (doc : XDocument) =
        doc.XPathSelectElements("atom:feed//atom:entry", ns)
        |> Seq.map (fun e ->
               { subscriptionId = Guid.Empty
                 id = e.XPathSelectElement("atom:id", ns).Value
                 title = e.XPathSelectElement("atom:title", ns).Value
                 uri = e.XPathSelectElement("atom:link", ns).Attribute("href" |> XName.op_Implicit).Value |> Uri })
        |> Seq.toList

    let getNodes (html : String) =
        html
        |> XDocument.Parse
        |> (fun doc -> parseRss doc @ parseAtom doc)

    let isValid =
        getNodes
        >> List.isEmpty
        >> not

let isValid (uri : Uri) =
    uri
    |> Http.download
    >>- Parser.isValid
let getNodes (uri : Uri) =
    uri
    |> Http.download
    >>- Parser.getNodes
