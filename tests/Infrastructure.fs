module Infrastructure

open System.IO
open System.Xml
open System.Xml.Linq
open System.Xml.XPath

let loadDocFromDisk name = 
    name
    |> sprintf "../../../examples/%s"
    |> File.ReadAllText
    |> XDocument.Parse