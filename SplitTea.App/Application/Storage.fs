module Storage

open Fable.Core
open Fable.Core.JsInterop
open SplitTea.Core

// ─── Primitive helpers ────────────────────────────────────────────────────────

let private guidStr (g: System.Guid) = string g

let private groupIdStr      (GroupId      g) = guidStr g
let private memberIdStr     (MemberId     g) = guidStr g
let private expenseIdStr    (ExpenseId    g) = guidStr g
let private settlementIdStr (SettlementId g) = guidStr g
let private eventIdStr      (EventId      g) = guidStr g

let private parseMemberId     (s: string) = MemberId     (System.Guid.Parse s)
let private parseExpenseId    (s: string) = ExpenseId    (System.Guid.Parse s)
let private parseSettlementId (s: string) = SettlementId (System.Guid.Parse s)
let private parseGroupId      (s: string) = GroupId      (System.Guid.Parse s)
let private parseEventId      (s: string) = EventId      (System.Guid.Parse s)
let private parseUserId       (s: string) = UserId       (System.Guid.Parse s)

let private isoStr (dto: System.DateTimeOffset) : string =
    let d : obj = unbox dto
    d?toISOString()

let private parseDte (s: string) = System.DateTimeOffset.Parse(s)

let private dateStr (d: System.DateOnly) =
    sprintf "%04d-%02d-%02d" d.Year d.Month d.Day

let private parseDate (s: string) =
    let parts = s.Split('-')
    System.DateOnly(int parts.[0], int parts.[1], int parts.[2])

let private decStr  (d: decimal) = string d
let private parseDec (s: string) = System.Decimal.Parse(s)

// Avoid opening Fable.Core.JS which shadows F# Array and Map modules.
let private objKeys (o: obj) : string list =
    Fable.Core.JS.Constructors.Object.keys(o) |> Seq.toList

// ─── Split encode / decode ────────────────────────────────────────────────────

let private encSplit (split: Split) : obj =
    let strShares (m: Map<MemberId, decimal>) =
        m |> Map.toList |> List.map (fun (k, v) -> memberIdStr k ==> decStr v) |> createObj
    let intShares (m: Map<MemberId, int>) =
        m |> Map.toList |> List.map (fun (k, v) -> memberIdStr k ==> box v)    |> createObj
    match split with
    | Equal members ->
        createObj [
            "type"    ==> "Equal"
            "members" ==> (members |> List.map memberIdStr |> Array.ofList)
        ]
    | Exact      shares -> createObj [ "type" ==> "Exact";      "shares" ==> strShares shares ]
    | Percentage shares -> createObj [ "type" ==> "Percentage"; "shares" ==> strShares shares ]
    | Shares     shares -> createObj [ "type" ==> "Shares";     "shares" ==> intShares shares ]

let private decSplit (raw: obj) : Split =
    let readStrShares (s: obj) =
        objKeys s
        |> List.map (fun k -> parseMemberId k, parseDec (string s?(k)))
        |> Map.ofList
    let readIntShares (s: obj) =
        objKeys s
        |> List.map (fun k -> parseMemberId k, int (string s?(k)))
        |> Map.ofList
    match string raw?``type`` with
    | "Equal" ->
        Equal (raw?members |> unbox<string[]> |> Array.toList |> List.map parseMemberId)
    | "Exact"      -> Exact      (readStrShares raw?shares)
    | "Percentage" -> Percentage (readStrShares raw?shares)
    | "Shares"     -> Shares     (readIntShares raw?shares)
    | t -> failwith $"Unknown split type: {t}"

// ─── Patch encode / decode ────────────────────────────────────────────────────
// Unchanged → null  |  Clear → "__CLEAR__"  |  SetTo v → encVal v

let private encodePatch (encVal: 'a -> obj) (patch: 'a Patch) : obj =
    match patch with
    | Unchanged -> null
    | Clear     -> box "__CLEAR__"
    | SetTo v   -> encVal v

let private decodePatch (decVal: obj -> 'a) (raw: obj) : 'a Patch =
    if isNull raw then Unchanged
    elif string raw = "__CLEAR__" then Clear
    else SetTo (decVal raw)

// ─── ContextId / ContextTemplate encode ──────────────────────────────────────

let private contextIdStr      (ContextId g) = guidStr g
let private parseContextId    (s: string)   = ContextId (System.Guid.Parse s)

let private encTemplate (t: ContextTemplate) : string =
    match t with
    | Trip    -> "Trip"
    | Monthly -> "Monthly"
    | Weekly  -> "Weekly"
    | Custom  -> "Custom"

let private decTemplate (s: string) : ContextTemplate =
    match s with
    | "Monthly" -> Monthly
    | "Weekly"  -> Weekly
    | "Custom"  -> Custom
    | _         -> Trip  // default / forward-compat

// ─── Member encode ────────────────────────────────────────────────────────────

let private encMember (m: Member) : obj =
    createObj [
        "id"          ==> memberIdStr m.Id
        "displayName" ==> m.DisplayName
        "userId"      ==> (m.UserId |> Option.map (fun (UserId g) -> guidStr g) |> Option.toObj)
    ]

let private decMember (raw: obj) : Member = {
    Id          = parseMemberId (string raw?id)
    DisplayName = string raw?displayName
    UserId      = if isNull raw?userId then None else Some (parseUserId (string raw?userId))
}

// ─── Payload encode / decode ──────────────────────────────────────────────────

let private encPayload (event: GroupEvent) : string * obj =
    match event with
    | GroupCreated env ->
        let p = env.Payload
        "GroupCreated", createObj [
            "name"      ==> p.Name
            "currency"  ==> p.Currency
            "createdBy" ==> memberIdStr p.CreatedBy
        ]
    | MemberAdded env ->
        "MemberAdded", createObj [ "member" ==> encMember env.Payload.Member ]
    | ContextCreated env ->
        let p = env.Payload
        "ContextCreated", createObj [
            "contextId" ==> contextIdStr p.ContextId
            "name"      ==> p.Name
            "template"  ==> encTemplate p.Template
            "members"   ==> (p.Members |> Option.map (fun ms -> ms |> List.map memberIdStr |> Array.ofList |> box) |> Option.toObj)
            "dateFrom"  ==> (p.DateFrom |> Option.map dateStr |> Option.toObj)
            "dateTo"    ==> (p.DateTo   |> Option.map dateStr |> Option.toObj)
        ]
    | ExpenseAdded env ->
        let p = env.Payload
        "ExpenseAdded", createObj [
            "expenseId"    ==> expenseIdStr p.ExpenseId
            "description"  ==> p.Description
            "paidAmount"   ==> decStr p.PaidAmount
            "paidCurrency" ==> p.PaidCurrency
            "exchangeRate" ==> (p.ExchangeRate |> Option.map decStr |> Option.toObj)
            "paidBy"       ==> memberIdStr p.PaidBy
            "split"        ==> encSplit p.Split
            "date"         ==> dateStr p.Date
            "category"     ==> (p.Category  |> Option.toObj)
            "notes"        ==> (p.Notes     |> Option.toObj)
            "contextId"    ==> (p.ContextId |> Option.map contextIdStr |> Option.toObj)
        ]
    | ExpenseCorrected env ->
        let p = env.Payload
        "ExpenseCorrected", createObj [
            "originalExpenseId" ==> expenseIdStr p.OriginalExpenseId
            "description"       ==> (p.Description  |> Option.toObj)
            "paidAmount"        ==> (p.PaidAmount    |> Option.map decStr      |> Option.toObj)
            "paidCurrency"      ==> (p.PaidCurrency  |> Option.toObj)
            "exchangeRate"      ==> encodePatch (decStr >> box) p.ExchangeRate
            "paidBy"            ==> (p.PaidBy        |> Option.map memberIdStr |> Option.toObj)
            "split"             ==> (p.Split         |> Option.map encSplit    |> Option.toObj)
            "date"              ==> (p.Date          |> Option.map dateStr     |> Option.toObj)
            "category"          ==> encodePatch box p.Category
            "notes"             ==> encodePatch box p.Notes
            "contextId"         ==> encodePatch (contextIdStr >> box) p.ContextId
            "reason"            ==> (p.Reason        |> Option.toObj)
        ]
    | ExpenseDeleted env ->
        let p = env.Payload
        "ExpenseDeleted", createObj [
            "expenseId" ==> expenseIdStr p.ExpenseId
            "reason"    ==> (p.Reason |> Option.toObj)
        ]
    | SettlementRecorded env ->
        let p = env.Payload
        "SettlementRecorded", createObj [
            "settlementId" ==> settlementIdStr p.SettlementId
            "from"         ==> memberIdStr p.From
            "to"           ==> memberIdStr p.To
            "amount"       ==> decStr p.Amount
            "currency"     ==> p.Currency
            "exchangeRate" ==> (p.ExchangeRate |> Option.map decStr |> Option.toObj)
            "date"         ==> dateStr p.Date
            "notes"        ==> (p.Notes |> Option.toObj)
        ]

// ─── Envelope extraction ──────────────────────────────────────────────────────

let private envFields (id: EventId) (gid: GroupId) (seq: int64) (aid: MemberId) (oat: System.DateTimeOffset) =
    eventIdStr id, groupIdStr gid, float seq, memberIdStr aid, isoStr oat

let private getEnvFields (event: GroupEvent) =
    match event with
    | GroupCreated       e -> envFields e.Id e.GroupId e.Sequence e.ActorId e.OccurredAt
    | MemberAdded        e -> envFields e.Id e.GroupId e.Sequence e.ActorId e.OccurredAt
    | ContextCreated     e -> envFields e.Id e.GroupId e.Sequence e.ActorId e.OccurredAt
    | ExpenseAdded       e -> envFields e.Id e.GroupId e.Sequence e.ActorId e.OccurredAt
    | ExpenseCorrected   e -> envFields e.Id e.GroupId e.Sequence e.ActorId e.OccurredAt
    | ExpenseDeleted     e -> envFields e.Id e.GroupId e.Sequence e.ActorId e.OccurredAt
    | SettlementRecorded e -> envFields e.Id e.GroupId e.Sequence e.ActorId e.OccurredAt

// ─── Serialise to JS object ───────────────────────────────────────────────────

let private toStored (event: GroupEvent) (synced: bool) : obj =
    let (eid, gid, seq, aid, oat) = getEnvFields event
    let (eventType, payload)      = encPayload event
    createObj [
        "id"         ==> eid
        "groupId"    ==> gid
        "sequence"   ==> seq
        "actorId"    ==> aid
        "occurredAt" ==> oat
        "eventType"  ==> eventType
        "payload"    ==> payload
        "synced"     ==> synced
    ]

// ─── Deserialise from JS object ───────────────────────────────────────────────

let private mkEnv<'P> (raw: obj) (payload: 'P) : EventEnvelope<'P> = {
    Id         = parseEventId  (string raw?id)
    GroupId    = parseGroupId  (string raw?groupId)
    Sequence   = int64 (float raw?sequence)
    ActorId    = parseMemberId (string raw?actorId)
    OccurredAt = parseDte      (string raw?occurredAt)
    CreatedAt  = parseDte      (string raw?occurredAt)
    Payload    = payload
}

let private eventSortKey (raw: obj) =
    let sequence = int64 (float raw?sequence)
    let occurredAt = string raw?occurredAt
    let eventId = string raw?id
    if sequence > 0L then
        0, sequence, "", eventId
    else
        1, 0L, occurredAt, eventId

let private fromStored (raw: obj) : GroupEvent option =
    let p : obj = raw?payload
    try
        match string raw?eventType with
        | "GroupCreated" ->
            GroupCreated (mkEnv raw {
                Name      = string p?name
                Currency  = string p?currency
                CreatedBy = parseMemberId (string p?createdBy)
            }) |> Some
        | "MemberAdded" ->
            MemberAdded (mkEnv raw { Member = decMember p?``member`` }) |> Some
        | "ContextCreated" ->
            ContextCreated (mkEnv raw {
                ContextId = parseContextId (string p?contextId)
                Name      = string p?name
                Template  = decTemplate (string p?template)
                Members   =
                    if isNull p?members then None
                    else Some (p?members |> unbox<string[]> |> Array.toList |> List.map parseMemberId)
                DateFrom  = if isNull p?dateFrom then None else Some (parseDate (string p?dateFrom))
                DateTo    = if isNull p?dateTo   then None else Some (parseDate (string p?dateTo))
            }) |> Some
        | "ExpenseAdded" ->
            // Migration: old events stored amount/currency; new events store paidAmount/paidCurrency/exchangeRate
            let isLegacy = isNull p?paidAmount
            ExpenseAdded (mkEnv raw {
                ExpenseId    = parseExpenseId (string p?expenseId)
                Description  = string p?description
                PaidAmount   = parseDec (string (if isLegacy then p?amount    else p?paidAmount))
                PaidCurrency = string  (if isLegacy then p?currency  else p?paidCurrency)
                ExchangeRate = if isNull p?exchangeRate then None else Some (parseDec (string p?exchangeRate))
                PaidBy       = parseMemberId (string p?paidBy)
                Split        = decSplit p?split
                Date         = parseDate (string p?date)
                Category     = if isNull p?category  then None else Some (string p?category)
                Notes        = if isNull p?notes     then None else Some (string p?notes)
                ContextId    = if isNull p?contextId then None else Some (parseContextId (string p?contextId))
            }) |> Some
        | "ExpenseCorrected" ->
            ExpenseCorrected (mkEnv raw {
                OriginalExpenseId = parseExpenseId (string p?originalExpenseId)
                Description  = if isNull p?description  then None else Some (string p?description)
                PaidAmount   = if isNull p?paidAmount   then None else Some (parseDec (string p?paidAmount))
                PaidCurrency = if isNull p?paidCurrency then None else Some (string p?paidCurrency)
                ExchangeRate = decodePatch (fun o -> parseDec (string o)) p?exchangeRate
                PaidBy       = if isNull p?paidBy       then None else Some (parseMemberId (string p?paidBy))
                Split        = if isNull p?split        then None else Some (decSplit p?split)
                Date         = if isNull p?date         then None else Some (parseDate (string p?date))
                Category     = decodePatch string p?category
                Notes        = decodePatch string p?notes
                ContextId    = decodePatch (fun o -> parseContextId (string o)) p?contextId
                Reason       = if isNull p?reason       then None else Some (string p?reason)
            }) |> Some
        | "ExpenseDeleted" ->
            ExpenseDeleted (mkEnv raw {
                ExpenseId = parseExpenseId (string p?expenseId)
                Reason    = if isNull p?reason then None else Some (string p?reason)
            }) |> Some
        | "SettlementRecorded" ->
            SettlementRecorded (mkEnv raw {
                SettlementId = parseSettlementId (string p?settlementId)
                From         = parseMemberId (string p?``from``)
                To           = parseMemberId (string p?``to``)
                Amount       = parseDec (string p?amount)
                Currency     = string p?currency
                ExchangeRate = if isNull p?exchangeRate then None else Some (parseDec (string p?exchangeRate))
                Date         = parseDate (string p?date)
                Notes        = if isNull p?notes then None else Some (string p?notes)
            }) |> Some
        | _ -> None
    with _ -> None

// ─── Public API ───────────────────────────────────────────────────────────────

let saveEvent (event: GroupEvent) : Async<unit> =
    IndexedDb.saveEvent (toStored event false)

let loadGroupState (groupId: GroupId) : Async<GroupState> =
    async {
        let! raws = IndexedDb.getEventsByGroup (groupIdStr groupId)
        let events =
            raws
            |> Array.sortBy eventSortKey
            |> Array.choose fromStored
            |> Array.toList
        return Reducer.replayEvents events
    }

let getPendingEvents () : Async<(GroupId * GroupEvent) list> =
    async {
        let! raws = IndexedDb.getPendingEvents ()
        return
            raws
            |> Array.sortBy eventSortKey
            |> Array.choose (fun r ->
                fromStored r |> Option.map (fun e -> parseGroupId (string r?groupId), e)
            )
            |> Array.toList
    }

let markSynced (eventId: EventId) : Async<unit> =
    IndexedDb.markSynced (eventIdStr eventId)
