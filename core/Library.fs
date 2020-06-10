namespace Spectator.Core

open System

[<AutoOpen>]
module Prelude =
    let inline ( *> ) ma mb = async { let! _ = ma
                                      return! mb }
    let inline (>>=) ma fm = async.Bind(ma, fm)
    let inline (>>-) ma f = async { let! a = ma
                                    return f a }
    let inline (>=>) mfa mfb a =
        async { let! b = mfa a
                let! c = mfb b
                return c }
    let inline flip f a b = f b a
    let inline curry f a b = f (a, b)
    let inline uncurry f (a, b) = f a b
    let inline uncurry' f (a, b, c) = f a b c
    let inline always a _ = a
    let inline pair a b = a, b
    let inline (@@) f x = f x
    let inline (^) f a = f a
    let inline (|||) a b =
        if String.IsNullOrEmpty a then b else a
    type Microsoft.FSharp.Control.AsyncBuilder with
        member __.Bind (t : System.Threading.Tasks.Task<'T>, f:'T -> Async<'R>) : Async<'R> =
            async.Bind(Async.AwaitTask t, f)
        member __.ReturnFrom (t : System.Threading.Tasks.Task<'T>) : Async<'T> = 
            async.ReturnFrom(Async.AwaitTask t)
    let [<System.Obsolete>] TODO() = raise ^ System.NotImplementedException()
    let (|Regex|_|) pattern input =
        let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None
    module List =
        let inline isNotEmpty xs = List.isEmpty xs |> not
    module Async =
        let inline map f a = async.Bind(a, f >> async.Return)
        let inline catch a = a |> Async.Catch |> map (function Choice1Of2 x -> Ok x | Choice2Of2 e -> Error e)
    module Pair =
        let inline map f (a, b) = f a, b
        let inline map2 f (a, b) = a, f b
    type Log() =
        static member log (message : string,
                           [<System.Runtime.CompilerServices.CallerFilePath;
                             System.Runtime.InteropServices.Optional;
                             System.Runtime.InteropServices.DefaultParameterValue("")>] file : string,
                           [<System.Runtime.CompilerServices.CallerLineNumber;
                             System.Runtime.InteropServices.Optional;
                             System.Runtime.InteropServices.DefaultParameterValue(0)>] line : int) =
            printfn "LOG %s:%i :: %s" file line message
        static member elog (message : string,
                           [<System.Runtime.CompilerServices.CallerFilePath;
                             System.Runtime.InteropServices.Optional;
                             System.Runtime.InteropServices.DefaultParameterValue("")>] file : string,
                           [<System.Runtime.CompilerServices.CallerLineNumber;
                             System.Runtime.InteropServices.Optional;
                             System.Runtime.InteropServices.DefaultParameterValue(0)>] line : int) =
            eprintfn "LOG (ERROR) %s:%i :: %s" file line message

module Async =
    let wrapTask (f : unit -> System.Threading.Tasks.Task) = async {
        do! f() |> Async.AwaitTask }
    let rec seq =
        function
        | [] -> async.Return []
        | h :: t ->
            async {
                let! b = h |> Async.Catch >>- function | Choice1Of2 x -> Ok x | Choice2Of2 x -> Error x
                let! c = seq t
                return b :: c }

module String =
    let isNullOrEmpty = String.IsNullOrEmpty
    let split (x : String) (separator : Char) = x.Split(separator) |> Array.toList

module List =
    let inline exceptBy xs compare origin =
        origin
        |> List.filter ^ fun x -> xs |> List.exists (fun y -> compare x y) |> not
