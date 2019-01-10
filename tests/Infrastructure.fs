module Infrastructure

open System.IO
open System.Xml.Linq

let loadFromDisk = sprintf "../../../examples/%s" >> File.ReadAllText

let loadDocFromDisk name =
    name
    |> loadFromDisk
    |> XDocument.Parse
