namespace Spectator.Worker
open System
open System.Net.Http
open System.Xml
open System.Xml.Linq
open System.Xml.XPath
open Spectator.Core

module RssParser =
    let private ns = XmlNamespaceManager(NameTable())
    ns.AddNamespace("media", "http://search.yahoo.com/mrss/")
    ns.AddNamespace("atom", "http://www.w3.org/2005/Atom")

    let parseRss (doc: XDocument) = []
    let parseAtom (doc: XDocument) = 
        doc.XPathSelectElements("atom:feed//atom:entry", ns)
        |> Seq.map (fun e -> 
            { subscriptionId = Guid.Empty
              title = e.XPathSelectElement("atom:title", ns).Value
              uri  = e.XPathSelectElement("atom:id", ns).Value |> Uri })
        |> Seq.toList
    let parseDocument (doc: XDocument) =
        if doc.XPathSelectElement("//channel/item") <> null then parseRss doc
        else if doc.XPathSelectElement("atom:feed//atom:entry", ns) <> null then parseAtom doc
        else []

type RssNodeProvider() =
    interface INodeProvider with
        member this.IsValid uri = async {
            use client = new HttpClient()
            let! text = client.GetStringAsync(uri) |> Async.AwaitTask
            return XDocument.Parse text |> RssParser.parseDocument |> List.isEmpty |> not
        }
        member this.Guid = Guid("3ACF3EB5-00BE-4332-9AAA-5D2F71F603F1")
        member this.GetNodes uri = async {
            use client = new HttpClient()
            let! text = client.GetStringAsync uri |> Async.AwaitTask
            return XDocument.Parse text |> RssParser.parseDocument
        }
        member this.Bind node = node