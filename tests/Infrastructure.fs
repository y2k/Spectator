module Infrastructure

open System.IO
open System.Xml.Linq

let loadDocFromDisk name = 
    name
    |> sprintf "../../../examples/%s"
    |> File.ReadAllText
    |> XDocument.Parse