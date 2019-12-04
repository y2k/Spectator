module Spectator.Core

open System

[<AutoOpen>]
module Operators =
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

module Async =
    let wrapTask (f : unit -> System.Threading.Tasks.Task) =
        async {
            do! f() |> Async.AwaitTask
        }
    let rec seq = function
        | [] -> async.Return []
        | h :: t ->
            async {
                let! b = h |> Async.Catch >>- function | Choice1Of2 x -> Ok x | Choice2Of2 x -> Error x
                let! c = seq t
                return b :: c
            }
    let lift = async.Return
    let map2 f a = async { let! (a1, a2) = a
                           return f a1 a2 }
    let map3 f a = async { let! (a1, a2, a3) = a
                           return f a1 a2 a3 }
    let zip a1 a2 f =
        async { let! r1 = a1
                let! r2 = a2
                return f (r1, r2) }
    let replaceWith x a = async { let! _ = a
                                  return x }
    let next a2 a = async { let! _ = a
                            return! a2 }

module Http =
    open System.Net.Http

    let download (uri : Uri) =
        async {
            use client = new HttpClient()
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/605.1.15 (KHTML, like Gecko) Chrome/71.0.3578.98 Safari/605.1 Edge/19.17763"
            |> client.DefaultRequestHeaders.UserAgent.ParseAdd
            return! client.GetStringAsync uri |> Async.AwaitTask
        }

module String =
    let isNullOrEmpty = String.IsNullOrEmpty
    let split (x : String) (separator : Char) = x.Split(separator) |> Array.toList

// Types

type LogList<'a> = LogList of 'a list
    with member this.unwrap = match this with LogList x -> x

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

type SubscriptionId = Guid

type Provider =
    | Invalid = 0
    | Rss = 1
    | Telegram = 2
    | Html = 3

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
      provider : Provider
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
      snapshots : Snapshot LogList }

type CoEffect<'a> = (CoEffectDb -> CoEffectDb * 'a) -> 'a Async

// Interfaces

type TelegramConnectorApi =
    abstract isValid : Uri -> bool Async
    abstract getNodes : Uri -> Snapshot list Async
    abstract resetClient : bool Async
    abstract updateToken : string -> unit Async

let mutable sTelegramApi : TelegramConnectorApi =
    { new TelegramConnectorApi with
          member __.isValid _ = async.Return false
          member __.getNodes _ = async.Return []
          member __.resetClient = async.Return false
          member __.updateToken _ = async.Zero() }
