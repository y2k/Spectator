namespace Spectator.Worker

open System
open Spectator.Core

type INodeProvider =
    abstract member Guid: Guid
    abstract member GetNodes: Uri -> Async<Snapshot list>
    abstract member Bind: Snapshot -> Snapshot
    abstract member IsValid: Uri -> Async<bool>