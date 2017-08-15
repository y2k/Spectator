module Spectator.Worker.RssParser

open System
open System.Net.Http
open System.Xml
open System.Xml.Linq
open System.Xml.XPath
open Spectator.Core

let private ns = XmlNamespaceManager(NameTable())
ns.AddNamespace("media", "http://search.yahoo.com/mrss/")
ns.AddNamespace("atom", "http://www.w3.org/2005/Atom")

let parseRss (doc : XDocument) = 
    doc.XPathSelectElements "//channel/item"
    |> Seq.map (fun e -> 
           { subscriptionId = e.XPathSelectElement("guid").Value
             title = e.XPathSelectElement("title").Value
             uri = e.XPathSelectElement("link").Value |> Uri })
    |> Seq.toList

let parseAtom (doc : XDocument) = 
    doc.XPathSelectElements("atom:feed//atom:entry", ns)
    |> Seq.map (fun e -> 
           { subscriptionId = e.XPathSelectElement("atom:id", ns).Value
             title = e.XPathSelectElement("atom:title", ns).Value
             uri = e.XPathSelectElement("atom:link", ns).Attribute("href" |> XName.op_Implicit).Value |> Uri })
    |> Seq.toList

let parseDocument (doc : XDocument) = 
    if doc.XPathSelectElement("//channel/item") <> null then parseRss doc
    else if doc.XPathSelectElement("atom:feed//atom:entry", ns) <> null then parseAtom doc
    else []

let isValid (uri: Uri) = async {
    use client = new HttpClient()
    let! text = client.GetStringAsync uri |> Async.AwaitTask
    return XDocument.Parse text |> parseDocument |> List.isEmpty |> not
}

let getNodes (uri: Uri) = async {
    use client = new HttpClient()
    let! text = client.GetStringAsync uri |> Async.AwaitTask
    return XDocument.Parse text |> parseDocument
}