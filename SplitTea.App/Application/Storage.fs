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

// ─── Rebase ───────────────────────────────────────────────────────────────────

type ConflictedEvent = {
    EventId     : EventId
    Description : string
    Reason      : string
}

let private eventId = function
    | SpaceCreated e       -> e.Id
    | SpaceRenamed e       -> e.Id
    | MemberAdded e        -> e.Id
    | MemberRenamed e      -> e.Id
    | CategoryAdded e      -> e.Id
    | CategoryRenamed e    -> e.Id
    | CategoryArchived e   -> e.Id
    | ExpenseAdded e       -> e.Id
    | ExpenseCorrected e   -> e.Id
    | ExpenseDeleted e     -> e.Id
    | SettlementRecorded e -> e.Id

let private describeEvent = function
    | SpaceCreated _       -> "Create space"
    | SpaceRenamed _       -> "Rename space"
    | MemberAdded _        -> "Add member"
    | MemberRenamed _      -> "Rename member"
    | CategoryAdded _      -> "Add category"
    | CategoryRenamed _    -> "Rename category"
    | CategoryArchived _   -> "Archive category"
    | ExpenseAdded _       -> "Add expense"
    | ExpenseCorrected _   -> "Edit expense"
    | ExpenseDeleted _     -> "Delete expense"
    | SettlementRecorded _ -> "Record settlement"

let private describeError = function
    | UnknownMember _         -> "Member no longer exists"
    | UnknownExpense _        -> "Expense no longer exists"
    | UnknownCategory _       -> "Category no longer exists"
    | ArchivedCategory _      -> "Category has been archived"
    | DeletedExpense _        -> "Expense has already been deleted"
    | AmountMustBePositive    -> "Amount must be positive"
    | SplitMustHaveMembers    -> "Split must include at least one member"
    | ExactSplitSumMismatch _ -> "Split amounts do not add up to expense total"
    | PercentageSumMismatch _ -> "Percentages do not add up to 100%"
    | SharesMustBePositive    -> "All shares must be positive"
    | SelfSettlement          -> "Cannot settle with yourself"
    | CurrencyMismatch _      -> "Currency mismatch"
    | ActorNotMember _        -> "You are no longer a member of this space"
    | CannotRenameOtherMember -> "You can only rename yourself"
    | DuplicateMember _       -> "A member with this ID already exists"

// Computes display state by rebasing pending local events on top of server state.
// Events that fail validation are marked conflicted in IndexedDB and returned as ConflictedEvent list.
let rebaseAndDisplay (spaceId: SpaceId) : Async<SpaceState * ConflictedEvent list> =
    async {
        let! raws = IndexedDb.getEventsBySpace (spaceIdStr spaceId)
        let synced  = raws |> Array.filter (fun r -> string r?synced = "true")
                           |> Array.sortBy eventSortKey
                           |> Array.choose fromStored
                           |> Array.toList
        let pending = raws |> Array.filter (fun r -> r?synced = box false)
                           |> Array.sortBy eventSortKey
                           |> Array.choose fromStored
                           |> Array.toList
        let (displayState, rebaseConflicts) = Rebase.apply synced pending
        let conflicts = ResizeArray()
        for rc in rebaseConflicts do
            let id = eventId rc.Event
            do! IndexedDb.markConflicted (eventIdStr id)
            conflicts.Add {
                EventId     = id
                Description = describeEvent rc.Event
                Reason      = rc.Errors |> List.map describeError |> String.concat "; "
            }
        return displayState, conflicts |> Seq.toList
    }

// ─── Known-space registry (localStorage) ─────────────────────────────────────

type SpaceSummary = { Id: SpaceId; Name: string }

let private knownSpacesKey = "knownSpaces"

let private loadKnownSpaces () : SpaceSummary list =
    let ls : obj = emitJsExpr () "window.localStorage"
    let raw : obj = ls?getItem(knownSpacesKey)
    if isNull raw then []
    else
        try
            let arr = jsonParse (string raw) |> unbox<obj[]>
            arr |> Array.choose (fun o ->
                try Some { Id = SpaceId (System.Guid.Parse (string o?id)); Name = string o?name }
                with _ -> None)
            |> Array.toList
        with _ -> []

let private saveKnownSpaces (summaries: SpaceSummary list) : unit =
    let arr =
        summaries
        |> List.map (fun s ->
            let (SpaceId g) = s.Id
            createObj [ "id" ==> string g; "name" ==> s.Name ])
        |> List.toArray
        |> box
    let ls : obj = emitJsExpr () "window.localStorage"
    ls?setItem(knownSpacesKey, jsonStringify arr) |> ignore

let getKnownSpaces () : SpaceSummary list = loadKnownSpaces ()

let upsertKnownSpace (id: SpaceId) (name: string) : unit =
    let existing = loadKnownSpaces ()
    let updated =
        if existing |> List.exists (fun s -> s.Id = id) then
            existing |> List.map (fun s -> if s.Id = id then { s with Name = name } else s)
        else
            existing @ [{ Id = id; Name = name }]
    saveKnownSpaces updated

let removeKnownSpace (id: SpaceId) : unit =
    loadKnownSpaces () |> List.filter (fun s -> s.Id <> id) |> saveKnownSpaces

let deleteSpace (spaceId: SpaceId) : Async<unit> =
    async {
        do! IndexedDb.deleteEventsBySpace (spaceIdStr spaceId)
        removeKnownSpace spaceId
        // Revoke server-side access. Best-effort: local data is already gone.
        try
            do! SupabaseSync.removeSpaceAccess (spaceIdStr spaceId)
        with ex ->
            Fable.Core.JS.console.error ("[SplitTea] deleteSpace: server access revocation failed", ex.Message)
    }
