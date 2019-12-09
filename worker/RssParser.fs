module Spectator.Worker.RssParser

open Spectator.Core

module Parser =
    open System
    open System.Xml
    open System.Xml.Linq
    open System.Xml.XPath

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

    let getNodes (html : string) =
        html
        |> XDocument.Parse
        |> (fun doc -> parseRss doc @ parseAtom doc)

    let isValid = getNodes >> List.isEmpty >> not

module private Http =
    open System.Net.Http

    let download (uri : System.Uri) = async {
        use client = new HttpClient()
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/605.1.15 (KHTML, like Gecko) Chrome/71.0.3578.98 Safari/605.1 Edge/19.17763"
        |> client.DefaultRequestHeaders.UserAgent.ParseAdd
        return! client.GetStringAsync uri |> Async.AwaitTask }

let RssParse =
    { new Spectator.Worker.HtmlProvider.IParse with
        member __.isValid uri = uri |> Http.download >>- Parser.isValid
        member __.getNodes uri = uri |> Http.download >>- Parser.getNodes }
