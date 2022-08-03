module Spectator.Worker.RssParser

open System
open Spectator.Core

module Parser =
    open System.Xml
    open System.Xml.Linq
    open System.Xml.XPath

    let pluginId = Guid.Parse "E5D3A9F2-325C-4CEF-BCA9-99D23F9E5AE5"

    let private nsHttp = XmlNamespaceManager(NameTable())
    nsHttp.AddNamespace("media", "http://search.yahoo.com/mrss/")
    nsHttp.AddNamespace("atom", "http://www.w3.org/2005/Atom")

    let private nsHttps = XmlNamespaceManager(NameTable())
    nsHttps.AddNamespace("media", "http://search.yahoo.com/mrss/")
    nsHttps.AddNamespace("atom", "https://www.w3.org/2005/Atom")

    let parseRss (doc: XDocument) =
        doc.XPathSelectElements "//channel/item"
        |> Seq.map
            (fun e ->
                { subscriptionId = TypedId.empty ()
                  created =
                      e.XPathSelectElement("pubDate").Value
                      |> DateTime.Parse
                  id = TypedId.empty ()
                  title = e.XPathSelectElement("title").Value
                  uri = e.XPathSelectElement("link").Value |> Uri })
        |> Seq.toList

    let parseAtom (ns: XmlNamespaceManager) (doc: XDocument) =
        doc.XPathSelectElements("atom:feed//atom:entry", ns)
        |> Seq.map
            (fun e ->
                { subscriptionId = TypedId.empty ()
                  created =
                      e.XPathSelectElement("atom:updated", ns).Value
                      |> DateTime.Parse
                  id = TypedId.empty ()
                  title = e.XPathSelectElement("atom:title", ns).Value
                  uri =
                      e
                          .XPathSelectElement("atom:link", ns)
                          .Attribute("href" |> XName.op_Implicit)
                          .Value
                      |> Uri })
        |> Seq.toList

    let getNodes (html: string) =
        html
        |> XDocument.Parse
        |> (fun doc ->
            parseRss doc
            @ parseAtom nsHttp doc @ parseAtom nsHttps doc)

    let isValid = getNodes >> List.isEmpty >> not

module Http =
    open System.Net.Http

    let download (uri: Uri) =
        async {
            use client = new HttpClient()

            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/605.1.15 (KHTML, like Gecko) Chrome/71.0.3578.98 Safari/605.1 Edge/19.17763"
            |> client.DefaultRequestHeaders.UserAgent.ParseAdd

            return! client.GetStringAsync uri
        }

let create downloadString =
    Parser.pluginId,
    (fun (uri: Uri) -> uri |> downloadString >>- Parser.isValid),
    (fun (uri: Uri) -> uri |> downloadString >>- Parser.getNodes)
