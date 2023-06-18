type Node = interface end

let reduceWebPart rules =
    let emptyNodeParse _ n = failwithf "No resolver for: %O" n
    let rec makeReducer (next: (Node -> _) -> Node -> _) (wp: Node) =
        next (makeReducer next) wp
    makeReducer (rules |> List.fold (fun a b -> b a) emptyNodeParse)

// ===============

type Choose = Choose of Node list
    with interface Node
type PathStarts = PathStarts of string
    with interface Node
type POST = POST
    with interface Node
type Combine = Combine of Node * Node
    with interface Node
let (>=>) a b : Node = Combine(a, b)

let toXmlBase next root (wp: Node) =
    match wp with
    | :? Combine as Combine (l, r) -> "<combine>" + root l + root r + "</combine>"
    | :? POST -> "<post/>"
    | :? PathStarts as PathStarts p -> sprintf "<pathStarts path='%s' />" p
    | :? Choose as Choose xs -> "<choose>" + (xs |> List.fold (fun a x -> a + root x + "\n") "") + "</choose>"
    | _ -> next root wp

// ===============

type Log = Log of string
    with interface Node

let toXml next root (wp: Node) =
    match wp with
    | :? Log as Log msg -> sprintf "<log value='%s' />" msg
    | _ -> next root wp

// ===============

type Auth = Auth of string * string
    with interface Node

type Choose2 = Choose2 of Node list
    with interface Node

let toXml2 next root (wp: Node) =
    match wp with
    | :? Auth as Auth (u, p) -> sprintf "<auth user='%s' pass='%s' />" u p
    | :? Choose2 as Choose2 xs -> "<choose2>" + (xs |> List.fold (fun a x -> a + root x) "") + "</choose2>"
    | _ -> next root wp

// ===============

Choose
    [ POST >=> Log "foo"
      Choose2
          [ Auth("user", "pass")
            POST >=> Choose2
                        [ PathStarts "/api"
                          Auth("user", "pass") ] ] ]
|> reduceWebPart [ toXmlBase; toXml; toXml2 ]
|> printfn "%O"
