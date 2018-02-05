module Spectator.Core

module Async = 
    let lift = async.Return
    let bind f a = 
        async {
            let! x = a
            return! f x
        }
    let map f a = 
        async {
            let! x = a
            return f x
        }
    let bindAll (f : 'a -> Async<'b>) (xsa : Async<'a list>) : Async<'b list> = 
        async { 
            let! xs = xsa
            return! xs
                    |> List.map f
                    |> Async.Parallel
                    |> map Array.toList
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

module Utils =
    let flip f a b = f b a

open System

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
    | GetNewSubscriptions
    | CreateSubscriptions of (Uri * Provider) list
    | GetSubscriptions
    | AddSnapshotsForSubscription of Snapshot list * Subscription
    | AddSubscription // TODO:
    | AddSnapshot // TODO:
    | AddNewSubscription of UserId * Uri
    | GetUserSubscriptions of UserId

type Responses = 
    | Subscriptions of Subscription list
    | NewSubscriptions of NewSubscription list
    | UserSubscriptions of NewSubscription list * Subscription list
    | SubscriptionCreatedSuccessfull
    | Unit
    | NotCalledStub

module Auth = 
    let computeAuthKey (user : UserId) (seed : string) = 
        let md5 = System.Security.Cryptography.MD5.Create()
        seed + user
        |> System.Text.Encoding.UTF8.GetBytes
        |> md5.ComputeHash
        |> System.Convert.ToBase64String