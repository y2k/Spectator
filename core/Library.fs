module Spectator.Core

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
    { subscriptionId : string
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

module Auth = 
    let computeAuthKey (user : UserId) (seed : string) = 
        let md5 = System.Security.Cryptography.MD5.Create()
        seed + user
        |> System.Text.Encoding.UTF8.GetBytes
        |> md5.ComputeHash
        |> System.Convert.ToBase64String