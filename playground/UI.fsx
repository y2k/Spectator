#r "nuget: SodiumFRP.FSharp, 5.0.6"

open Sodium.Frp

module FsHtml =
    type Element =
        | Element of string * (string * obj) list * Element list
        | TextElement of string

    let div attrs children = Element("div", attrs, children)
    let input attrs children = Element("input", attrs, children)
    let span attrs children = Element("span", attrs, children)
    let button attrs children = Element("button", attrs, children)
    let attr (k: string) (v: obj) = k, v
    let str text = TextElement text

    let rec toString =
        function
        | TextElement x -> x
        | (Element(name, attrs, children)) ->
            let textAttrs =
                attrs
                |> List.choose (fun (k, v) ->
                    match v with
                    | :? string as s -> Some(sprintf "%s=\"%s\"" k s)
                    | _ -> None)
                |> function
                    | [] -> ""
                    | xs -> xs |> List.reduce (sprintf "%s %s")

            let textChildren =
                children
                |> List.map toString
                |> function
                    | [] -> ""
                    | xs -> xs |> List.reduce (sprintf "%s %s")

            sprintf "<%s %s>%s</%s>" name textAttrs textChildren name

open FsHtml

let viewCount (count: int) (onInc: unit -> unit) (onDec: unit -> unit) =
    div
        []
        [ button [ "click", box (fun _ -> onInc ()) ] [ str "+" ]
          span [] [ str (string count) ]
          button [ "click", box (fun _ -> onDec ()) ] [ str "-" ] ]

let rootView _ =
    div
        []
        [ viewCount (failwith "???") (failwith "???") (failwith "???")
          viewCount (failwith "???") (failwith "???") (failwith "???") ]

let update _ =
    let plusClicked = StreamSink.create<unit> ()
    let minusClicked = StreamSink.create<unit> ()

    let count =
        [ plusClicked |> Stream.map (fun _ s -> s + 1)
          minusClicked |> Stream.map (fun _ s -> s - 1) ]
        |> Stream.mergeAll (fun x _ -> x)
        |> Stream.accum 0 (fun f s -> f s)

    ()
