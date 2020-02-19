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
    let inline (^) f a = f a
    let inline (|||) a b =
        if String.IsNullOrEmpty a then b else a
    let (|Regex|_|) pattern input =
        let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None
    module Result =
        let unwrap = function Ok x -> x | Error e -> failwith e
    type Microsoft.FSharp.Control.AsyncBuilder with
        member __.Bind (t : System.Threading.Tasks.Task<'T>, f:'T -> Async<'R>) : Async<'R> =
            async.Bind(Async.AwaitTask t, f)
        member __.ReturnFrom (t : System.Threading.Tasks.Task<'T>) : Async<'T> = 
            async.ReturnFrom(Async.AwaitTask t)

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

module EnvironmentConfig =
    type TelegramType =
        { Proxy : string
          Auth : string
          Token : string }
    type Root =
        { Telegram : TelegramType
          TelegramAdmin : string
          FilesDir : string
          MongoDomain : string }

type UserId = string
type SubscriptionId = SubscriptionId of Guid
type PluginId = PluginId of Guid

[<CLIMutable>]
type NewSubscription =
    { id : SubscriptionId
      userId : UserId
      uri : Uri
      filter : string }

[<CLIMutable>]
type Subscription =
    { id : SubscriptionId
      userId : UserId
      provider : PluginId
      uri : Uri
      filter : string }

type Snapshot =
    { subscriptionId : SubscriptionId
      id : string
      title : string
      uri : Uri }

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
