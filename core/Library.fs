module Spectator.Core

open System

[<AutoOpen>]
module Operators =
    let inline (>>=) ma fm = async.Bind(ma, fm)
    let inline (>>-) ma f = 
        async {
            let! a = ma
            return f a
        }
    let inline (>=>) mfa mfb a =
        async {
            let! b = mfa a
            let! c = mfb b
            return c
        }
    let inline flip f a b = f b a
    let inline curry f a b = f (a, b)
    let inline uncurry f (a, b) = f a b
    let inline always a _ = a

module Async = 
    let lift = async.Return
    let map2 f a = 
        async {
            let! (a1, a2) = a
            return f a1 a2
        }
    let map3 f a = 
        async {
            let! (a1, a2, a3) = a
            return f a1 a2 a3
        }
    let bindAll (f : 'a -> Async<'b>) (xsa : Async<'a list>) : Async<'b list> = 
        async { 
            let! xs = xsa
            return! xs
                    |> List.map f
                    |> Async.Parallel
                    >>- Array.toList
        }
    let zip a1 a2 f = 
        async { 
            let! r1 = a1
            let! r2 = a2
            return f (r1, r2) 
        }
    let replaceWith x a = 
        async { 
            let! _ = a
            return x 
        }
    let next a2 a = 
        async { 
            let! _ = a
            return! a2
        }

module Http =
    open System.Net.Http
    let download (uri : Uri) =
        async {
            use client = new HttpClient()
            return! client.GetStringAsync uri |> Async.AwaitTask
        }

module String =
    let split (x : String) (separator : Char) = x.Split(separator) |> Array.toList

// Types
type UserId = string
type SubscriptionId = Guid

type Provider = 
    | Invalid = 0
    | Rss = 1

type NewSubscription = 
    { userId : UserId
      uri : Uri }

type Subscription = 
    { id : SubscriptionId
      userId : UserId
      provider : Provider
      uri : Uri }

type Snapshot = 
    { subscriptionId : SubscriptionId
      id : string
      title : string
      uri : Uri }

// EasyNetQ Commands
type Command = 
    | AddSubscription // TODO:
    | AddSnapshot // TODO:
    | Ping
    | AddSnapshotsForSubscription of Snapshot list * Subscription * AsyncReplyChannel<unit>
    | GetAllSubscriptions of AsyncReplyChannel<Subscription list>
    | CreateSubscription of UserId * Uri * Provider * AsyncReplyChannel<unit>
    | GetAllNewSubscriptions of AsyncReplyChannel<NewSubscription list>
    | GetUserSubscriptions of UserId * AsyncReplyChannel<NewSubscription list * Subscription list>
    | AddNewSubscription of UserId * Uri * AsyncReplyChannel<unit>

module Auth = 
    let computeAuthKey (user : UserId) (seed : string) = 
        let md5 = System.Security.Cryptography.MD5.Create()
        seed + user
        |> System.Text.Encoding.UTF8.GetBytes
        |> md5.ComputeHash
        |> System.Convert.ToBase64String

module Bus =
    let reply (bus' : MailboxProcessor<Command>) f =
        bus'.PostAndAsyncReply (fun x -> f x)
