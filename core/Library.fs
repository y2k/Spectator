module Spectator.Core

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

// Types

type EventLog<'a> =
    | EventLog of 'a list
    member this.unwrap = match this with | EventLog x -> x

type UserId = string
type PluginId = Guid

[<MeasureAnnotatedAbbreviation>]
type 'a TypedId = Guid
#nowarn "42"
module TypedId =
    let inline wrap<'b> (a : Guid) : 'b TypedId = (# "" a : TypedId<'b> #)

[<CLIMutable>]
type Subscription =
    { id : Subscription TypedId
      userId : UserId
      provider : PluginId
      uri : Uri
      filter : string }
    static member empty = { id = TypedId.wrap Guid.Empty; userId = ""; provider = Guid.Empty; uri = null; filter = "" }

[<CLIMutable>]
type NewSubscription =
    { id : Subscription TypedId
      userId : UserId
      uri : Uri
      filter : string }
      static member empty = { id = TypedId.wrap Guid.Empty; userId = ""; uri = null; filter = "" }

[<CLIMutable>]
type Snapshot =
    { subscriptionId : Subscription TypedId
      id : string
      title : string
      uri : Uri }
    static member empty = { subscriptionId = TypedId.wrap Guid.Empty; id = ""; title = ""; uri = null }

module Auth =
    let computeAuthKey (user : UserId) (seed : string) =
        let md5 = System.Security.Cryptography.MD5.Create()
        seed + user
        |> System.Text.Encoding.UTF8.GetBytes
        |> md5.ComputeHash
        |> System.Convert.ToBase64String

module MongoCollections =
    let SnapshotsDb = "snapshots"
    let SubscriptionsDb = "subscriptions"
    let NewSubscriptionsDb = "newSubscriptions"

type CoEffectDb =
    { subscriptions : Subscription list
      newSubscriptions : NewSubscription list
      snapshots : Snapshot EventLog }
    static member empty = { subscriptions = []; newSubscriptions = []; snapshots = EventLog [] }

type CoEffect<'a> = (CoEffectDb -> CoEffectDb * 'a) -> 'a Async

module Cmd =
    let none = []

// Global effects

type IDbEff =
    abstract member run<'a> : (CoEffectDb -> CoEffectDb * 'a) -> 'a Async

module DependencyGraph =
    type Config =
        { filesDir : string
          mongoDomain : string
          restTelegramPassword : string
          restTelegramBaseUrl : string
          telegramToken : string }
    let mutable config = { filesDir = ""; mongoDomain = ""; restTelegramPassword = ""; restTelegramBaseUrl = ""; telegramToken = ""; }
    let mutable listenLogUpdates : (CoEffectDb -> unit Async) -> unit Async = fun _ -> failwith "not implemented"
    let mutable dbEff = { new IDbEff with member __.run _ = failwith "not implemented" }

module Eff =
    let rec runEffects (invoke : 'cmd -> _) update (msg : 'msg) =
        async {
            let! cmds = DependencyGraph.dbEff.run (update msg)
            cmds |> List.iter ^ printfn "Worker :: %O"
            for cmd in cmds do
                let! responses = invoke cmd
                return! responses |> runEffects invoke update
        }
