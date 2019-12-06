module Spectator.Worker.HtmlProvider

module private ShinglesTester =
    open Spectator.Core
    open System.Text.RegularExpressions

    let private checkLength = 32

    let private clear text =
        let text = Regex.Replace(text, "<style.+?</style>", "", RegexOptions.Singleline)
        let text = Regex.Replace(text, "<script.+?</script>", "", RegexOptions.Singleline)
        let text = Regex.Replace(text, "<!--.+?-->", "", RegexOptions.Singleline)
        let text = Regex.Replace(text, "\\<[^>]+\\>", "", RegexOptions.Singleline)
        let text = Regex.Replace(text, "[., \r\n\\d]+", "", RegexOptions.Singleline)
        text

    let private computeCodes (page : string) =
        [| for i in 0..page.Length - checkLength -> page.Substring(i, checkLength).GetHashCode() |]

    let private analize (source : int []) (dest : int []) =
        let same = source |> Array.sumBy ^ fun x -> if Array.contains x dest then 1 else 0
        (float same) * 2. / (float (source.Length + dest.Length))

    let compare sourceContent destContent =
        analize
            (sourceContent |> (clear >> computeCodes))
            (destContent |> (clear >> computeCodes))

open Spectator.Core
open System

type IParse =
    abstract member isValid : System.Uri -> bool Async
    abstract member getNodes : System.Uri -> Snapshot list Async

type F = System.IO.File

let private mkFilePath (env : EnvironmentConfig.Root) (uri : Uri) =
    IO.Path.Combine(env.FilesDir, sprintf "snapshot_%O.html" <| uri.GetHashCode())

let private compare env uri content = async {
    let path = mkFilePath env uri
    if F.Exists path then
        let oldContent = F.ReadAllText path
        return ShinglesTester.compare oldContent content
    else return 0.0 }

let private getTitle content =
    let doc = HtmlAgilityPack.HtmlDocument()
    doc.LoadHtml content
    doc.DocumentNode.SelectSingleNode("//title").InnerHtml

let private download (uri : Uri) = async {
    use http = new Net.Http.HttpClient()
    use! r = http.GetAsync uri |> Async.AwaitTask
    r.RequestMessage.Headers.UserAgent.ParseAdd "Mozilla/6.0 (Windows NT 6.2; WOW64; rv:16.0.1) Gecko/20121011 Firefox/16.0.1"
    r.RequestMessage.Headers.Accept.ParseAdd "text/html, application/xml;q=0.9, application/xhtml+xml, image/png, image/webp, image/jpeg, image/gif, image/x-xbitmap, */*;q=0.1"
    return! r.Content.ReadAsStringAsync() |> Async.AwaitTask }
    
type HtmlParse(env : EnvironmentConfig.Root) =
    interface IParse with
        member __.isValid uri = async { return uri.IsAbsoluteUri && uri.Scheme = "https" }
        member __.getNodes uri = async {
            let! content = download uri
            let! sim = compare env uri content
            if sim < 0.9 then
                F.WriteAllText(mkFilePath env uri, content)
                let snap =
                    { subscriptionId = Guid.Empty
                      id = sprintf "uri-%O" <| uri.GetHashCode()
                      title = getTitle content
                      uri = uri }
                return [ snap ]
            else return [] }
