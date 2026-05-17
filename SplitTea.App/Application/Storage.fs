module Storage

open Fable.Core
open Fable.Core.JsInterop
open SplitTea.Core

let private guidStr (g: System.Guid) = string g
let private spaceIdStr  (SpaceId  g) = guidStr g
let private memberIdStr (MemberId g) = guidStr g
let private eventIdStr  (EventId  g) = guidStr g
let private parseSpaceId (s: string) = SpaceId (System.Guid.Parse s)

let private jsonParse     (s: string) : obj    = emitJsExpr s "JSON.parse($0)"
let private jsonStringify (o: obj)    : string = emitJsExpr o "JSON.stringify($0)"

let private eventSortKey (raw: obj) =
    let sequence   = int64 (float raw?sequence)
    let occurredAt = string raw?occurredAt
    let eventId    = string raw?id
    if sequence > 0L then 0, sequence, "", eventId
    else               1, 0L, occurredAt, eventId

let private toStored (event: SpaceEvent) (synced: bool) : obj =
    let stored : obj = Serde.encodeEventJson event |> jsonParse
    stored?synced <- synced
    stored

let private fromStored (raw: obj) : SpaceEvent option =
    jsonStringify raw
    |> Serde.decodeEventJson
    |> Result.toOption

#if DEVMODE
let private devChannel : obj = emitJsExpr () "new BroadcastChannel('splittea-dev')"
#endif

// ─── Public API ───────────────────────────────────────────────────────────────

let saveEvent (event: SpaceEvent) : Async<unit> =
    async {
        let stored = toStored event false
        do! IndexedDb.saveEvent stored
#if DEVMODE
        if DevMode.isEnabled () then
            devChannel?postMessage(stored?spaceId) |> ignore
#endif
    }

let loadSpaceState (spaceId: SpaceId) : Async<SpaceState> =
    async {
        let! raws = IndexedDb.getEventsBySpace (spaceIdStr spaceId)
        let events =
            raws
            |> Array.sortBy eventSortKey
            |> Array.choose fromStored
            |> Array.toList
        return Reducer.replayEvents events
    }

let getPendingEvents () : Async<(SpaceId * SpaceEvent) list> =
    async {
        let! raws = IndexedDb.getPendingEvents ()
        return
            raws
            |> Array.sortBy eventSortKey
            |> Array.choose (fun r ->
                fromStored r |> Option.map (fun e -> parseSpaceId (string r?spaceId), e)
            )
            |> Array.toList
    }

let markSynced (eventId: EventId) : Async<unit> =
    IndexedDb.markSynced (eventIdStr eventId)
