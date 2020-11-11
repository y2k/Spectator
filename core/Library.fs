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
    let inline apply x f = f x
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
        member __.Bind (t : Threading.Tasks.Task<'T>, f:'T -> Async<'R>) : Async<'R> =
            async.Bind(Async.AwaitTask t, f)
        // member __.Bind (t : Threading.Tasks.Task, f:unit -> Async<'R>) : Async<'R> =
        //     async.Bind(Async.AwaitTask t, f)
        member __.Bind (t : Threading.Tasks.ValueTask<'T>, f:'T -> Async<'R>) : Async<'R> =
            async.Bind(Async.AwaitTask <| t.AsTask(), f)
        member __.ReturnFrom (t : Threading.Tasks.Task<'T>) : Async<'T> =
            async.ReturnFrom(Async.AwaitTask t)
    let [<Obsolete>] TODO() = raise ^ NotImplementedException()
    let (|Regex|_|) pattern input =
        let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None
    module List =
        let inline isNotEmpty xs = List.isEmpty xs |> not
    module Async =
        let inline map f a = async.Bind(a, f >> async.Return)
        let inline catch a = a |> Async.Catch |> map (function Choice1Of2 x -> Ok x | Choice2Of2 e -> Error e)
        let loopAll axs =
            let loop a =
                async {
                    while true do
                        do! a
                }
            axs
            |> List.map loop
            |> Async.Parallel
            |> Async.Ignore
    module Pair =
        let inline map f (a, b) = f a, b
        let inline map2 f (a, b) = a, f b
    module Result =
        let toOption = function Ok x -> Some x | Error _ -> None
    type Log() =
        static member log (message : string,
                           [<Runtime.CompilerServices.CallerFilePath;
                             Runtime.InteropServices.Optional;
                             Runtime.InteropServices.DefaultParameterValue("")>] file : string,
                           [<Runtime.CompilerServices.CallerLineNumber;
                             Runtime.InteropServices.Optional;
                             Runtime.InteropServices.DefaultParameterValue(0)>] line : int) =
            printfn "LOG %s:%i :: %s" file line message
        static member elog (message : string,
                           [<Runtime.CompilerServices.CallerFilePath;
                             Runtime.InteropServices.Optional;
                             Runtime.InteropServices.DefaultParameterValue("")>] file : string,
                           [<Runtime.CompilerServices.CallerLineNumber;
                             Runtime.InteropServices.Optional;
                             Runtime.InteropServices.DefaultParameterValue(0)>] line : int) =
            eprintfn "LOG (ERROR) %s:%i :: %s" file line message

module String =
    let isNullOrEmpty = String.IsNullOrEmpty
    let split (x : String) (separator : Char) = x.Split(separator) |> Array.toList

module List =
    let inline exceptBy xs compare origin =
        origin
        |> List.filter ^ fun x -> xs |> List.exists (fun y -> compare x y) |> not
