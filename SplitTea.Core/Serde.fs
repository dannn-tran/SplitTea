module SplitTea.Core.Serde

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

open SplitTea.Core

// ─── Primitive helpers ────────────────────────────────────────────────────────

let private encOpt enc = function None -> Encode.nil | Some v -> enc v

let private encGuid (g: System.Guid) = Encode.string (string g)
let private decGuid = Decode.string |> Decode.map System.Guid.Parse

let private encSpaceId      (SpaceId      g) = encGuid g
let private encMemberId     (MemberId     g) = encGuid g
let private encExpenseId    (ExpenseId    g) = encGuid g
let private encSettlementId (SettlementId g) = encGuid g
let private encEventId      (EventId      g) = encGuid g
let private encUserId       (UserId       g) = encGuid g

let private decSpaceId      = decGuid |> Decode.map SpaceId
let private decMemberId     = decGuid |> Decode.map MemberId
let private decExpenseId    = decGuid |> Decode.map ExpenseId
let private decSettlementId = decGuid |> Decode.map SettlementId
let private decEventId      = decGuid |> Decode.map EventId
let private decUserId       = decGuid |> Decode.map UserId

let private encDecimal (d: decimal) = Encode.string (string d)
let private decDecimal = Decode.string |> Decode.map decimal

let private encDateOnly (d: System.DateOnly) =
    Encode.string (sprintf "%04d-%02d-%02d" d.Year d.Month d.Day)
let private decDateOnly =
    Decode.string |> Decode.map (fun s ->
        let p = s.Split('-')
        System.DateOnly(int p.[0], int p.[1], int p.[2]))

let private encDateTimeOffset (dto: System.DateTimeOffset) = Encode.string (dto.ToString("O"))
let private decDateTimeOffset = Decode.string |> Decode.map System.DateTimeOffset.Parse

// ─── Split ───────────────────────────────────────────────────────────────────

let private encIdDecPair (m, v) =
    Encode.object [ "m", encMemberId m; "v", encDecimal v ]

let private encIdIntPair (m, v) =
    Encode.object [ "m", encMemberId m; "v", Encode.int v ]

let private decIdDecPair =
    Decode.map2 (fun m v -> m, v)
        (Decode.field "m" decMemberId)
        (Decode.field "v" decDecimal)

let private decIdIntPair =
    Decode.map2 (fun m v -> m, v)
        (Decode.field "m" decMemberId)
        (Decode.field "v" Decode.int)

let private encSplit (split: Split) =
    match split with
    | Equal members ->
        Encode.object [
            "type",    Encode.string "Equal"
            "members", Encode.list (members |> List.map encMemberId)
        ]
    | Exact shares ->
        Encode.object [ "type", Encode.string "Exact";      "shares", Encode.list (shares |> List.map encIdDecPair) ]
    | Percentage shares ->
        Encode.object [ "type", Encode.string "Percentage"; "shares", Encode.list (shares |> List.map encIdDecPair) ]
    | Shares shares ->
        Encode.object [ "type", Encode.string "Shares";     "shares", Encode.list (shares |> List.map encIdIntPair) ]

let private decSplit =
    Decode.field "type" Decode.string
    |> Decode.andThen (function
        | "Equal" ->
            Decode.field "members" (Decode.list decMemberId) |> Decode.map Equal
        | "Exact" ->
            Decode.field "shares" (Decode.list decIdDecPair) |> Decode.map Exact
        | "Percentage" ->
            Decode.field "shares" (Decode.list decIdDecPair) |> Decode.map Percentage
        | "Shares" ->
            Decode.field "shares" (Decode.list decIdIntPair) |> Decode.map Shares
        | t -> Decode.fail $"Unknown split type: {t}")

// ─── Patch ───────────────────────────────────────────────────────────────────
// Unchanged → null  |  Clear → "__CLEAR__"  |  SetTo v → encVal v

let private encPatch encVal = function
    | Unchanged -> Encode.nil
    | Clear     -> Encode.string "__CLEAR__"
    | SetTo v   -> encVal v

let private decPatch decVal =
    Decode.oneOf [
        Decode.nil Unchanged
        Decode.string |> Decode.andThen (fun s ->
            if s = "__CLEAR__" then Decode.succeed Clear
            else Decode.fail $"not a patch sentinel: {s}")
        decVal |> Decode.map SetTo
    ]

// ─── Member ──────────────────────────────────────────────────────────────────

let private encMember (m: Member) =
    Encode.object [
        "id",          encMemberId m.Id
        "displayName", Encode.string m.DisplayName
        "userId",      encOpt encUserId m.UserId
    ]

let private decMember =
    Decode.object (fun get -> {
        Id          = get.Required.Field "id"          decMemberId
        DisplayName = get.Required.Field "displayName" Decode.string
        UserId      = get.Optional.Field "userId"      decUserId
    })

// ─── Payload encoders ─────────────────────────────────────────────────────────

let private encSpaceCreated (p: SpaceCreatedPayload) =
    Encode.object [
        "name",       Encode.string p.Name
        "currency",   Encode.string p.Currency
        "createdBy",  encMemberId p.CreatedBy
        "categories", Encode.list (p.Categories |> List.map Encode.string)
    ]

let private encMemberAdded (p: MemberAddedPayload) =
    Encode.object [ "member", encMember p.Member ]

let private encCategoryAdded (p: CategoryAddedPayload) =
    Encode.object [ "name", Encode.string p.Name ]

let private encCategoryRenamed (p: CategoryRenamedPayload) =
    Encode.object [ "oldName", Encode.string p.OldName; "newName", Encode.string p.NewName ]

let private encCategoryArchived (p: CategoryArchivedPayload) =
    Encode.object [ "name", Encode.string p.Name ]

let private encSpaceRenamed (p: SpaceRenamedPayload) =
    Encode.object [ "newName", Encode.string p.NewName ]

let private encMemberRenamed (p: MemberRenamedPayload) =
    Encode.object [ "memberId", encMemberId p.MemberId; "newName", Encode.string p.NewName ]

let private encExpenseAdded (p: ExpenseAddedPayload) =
    Encode.object [
        "expenseId",    encExpenseId p.ExpenseId
        "description",  Encode.string p.Description
        "paidAmount",   encDecimal p.PaidAmount
        "paidCurrency", Encode.string p.PaidCurrency
        "exchangeRate", encOpt encDecimal p.ExchangeRate
        "paidBy",       encMemberId p.PaidBy
        "split",        encSplit p.Split
        "date",         encDateOnly p.Date
        "category",     encOpt Encode.string p.Category
        "notes",        encOpt Encode.string p.Notes
    ]

let private encExpenseCorrected (p: ExpenseCorrectedPayload) =
    Encode.object [
        "originalExpenseId", encExpenseId p.OriginalExpenseId
        "description",       encOpt Encode.string p.Description
        "paidAmount",        encOpt encDecimal p.PaidAmount
        "paidCurrency",      encOpt Encode.string p.PaidCurrency
        "exchangeRate",      encPatch encDecimal p.ExchangeRate
        "paidBy",            encOpt encMemberId p.PaidBy
        "split",             encOpt encSplit p.Split
        "date",              encOpt encDateOnly p.Date
        "category",          encPatch Encode.string p.Category
        "notes",             encPatch Encode.string p.Notes
        "reason",            encOpt Encode.string p.Reason
    ]

let private encExpenseDeleted (p: ExpenseDeletedPayload) =
    Encode.object [
        "expenseId", encExpenseId p.ExpenseId
        "reason",    encOpt Encode.string p.Reason
    ]

let private encSettlementRecorded (p: SettlementRecordedPayload) =
    Encode.object [
        "settlementId", encSettlementId p.SettlementId
        "from",         encMemberId p.From
        "to",           encMemberId p.To
        "amount",       encDecimal p.Amount
        "currency",     Encode.string p.Currency
        "exchangeRate", encOpt encDecimal p.ExchangeRate
        "date",         encDateOnly p.Date
        "notes",        encOpt Encode.string p.Notes
    ]

// ─── Payload decoders ─────────────────────────────────────────────────────────

let private decSpaceCreated =
    Decode.object (fun get -> {
        Name       = get.Required.Field "name"       Decode.string
        Currency   = get.Required.Field "currency"   Decode.string
        CreatedBy  = get.Required.Field "createdBy"  decMemberId
        Categories = get.Optional.Field "categories" (Decode.list Decode.string) |> Option.defaultValue []
    })

let private decMemberAdded =
    Decode.object (fun get -> { Member = get.Required.Field "member" decMember })

let private decCategoryAdded =
    Decode.object (fun get -> { Name = get.Required.Field "name" Decode.string } : CategoryAddedPayload)

let private decCategoryRenamed =
    Decode.object (fun get -> {
        OldName = get.Required.Field "oldName" Decode.string
        NewName = get.Required.Field "newName" Decode.string
    })

let private decCategoryArchived =
    Decode.object (fun get -> { Name = get.Required.Field "name" Decode.string } : CategoryArchivedPayload)

let private decSpaceRenamed =
    Decode.object (fun get -> { NewName = get.Required.Field "newName" Decode.string } : SpaceRenamedPayload)

let private decMemberRenamed =
    Decode.object (fun get -> {
        MemberId = get.Required.Field "memberId" decMemberId
        NewName  = get.Required.Field "newName"  Decode.string
    })

let private decExpenseAdded =
    Decode.object (fun get -> {
        ExpenseId    = get.Required.Field "expenseId"    decExpenseId
        Description  = get.Required.Field "description"  Decode.string
        PaidAmount   = get.Required.Field "paidAmount"   decDecimal
        PaidCurrency = get.Required.Field "paidCurrency" Decode.string
        ExchangeRate = get.Optional.Field "exchangeRate" decDecimal
        PaidBy       = get.Required.Field "paidBy"       decMemberId
        Split        = get.Required.Field "split"        decSplit
        Date         = get.Required.Field "date"         decDateOnly
        Category     = get.Optional.Field "category"     Decode.string
        Notes        = get.Optional.Field "notes"        Decode.string
    })

let private decExpenseCorrected =
    Decode.object (fun get -> {
        OriginalExpenseId = get.Required.Field "originalExpenseId" decExpenseId
        Description  = get.Optional.Field "description"  Decode.string
        PaidAmount   = get.Optional.Field "paidAmount"   decDecimal
        PaidCurrency = get.Optional.Field "paidCurrency" Decode.string
        ExchangeRate = get.Required.Field "exchangeRate" (decPatch decDecimal)
        PaidBy       = get.Optional.Field "paidBy"       decMemberId
        Split        = get.Optional.Field "split"        decSplit
        Date         = get.Optional.Field "date"         decDateOnly
        Category     = get.Required.Field "category"     (decPatch Decode.string)
        Notes        = get.Required.Field "notes"        (decPatch Decode.string)
        Reason       = get.Optional.Field "reason"       Decode.string
    })

let private decExpenseDeleted =
    Decode.object (fun get -> {
        ExpenseId = get.Required.Field "expenseId" decExpenseId
        Reason    = get.Optional.Field "reason"    Decode.string
    })

let private decSettlementRecorded =
    Decode.object (fun get -> {
        SettlementId = get.Required.Field "settlementId" decSettlementId
        From         = get.Required.Field "from"         decMemberId
        To           = get.Required.Field "to"           decMemberId
        Amount       = get.Required.Field "amount"       decDecimal
        Currency     = get.Required.Field "currency"     Decode.string
        ExchangeRate = get.Optional.Field "exchangeRate" decDecimal
        Date         = get.Required.Field "date"         decDateOnly
        Notes        = get.Optional.Field "notes"        Decode.string
    })

// ─── Event envelope ───────────────────────────────────────────────────────────

let private encEnvFields id spaceId seq actorId (at: System.DateTimeOffset) =
    [
        "id",         encEventId id
        "spaceId",    encSpaceId spaceId
        "sequence",   Encode.float (float seq)
        "actorId",    encMemberId actorId
        "occurredAt", encDateTimeOffset at
    ]

// inline so F# generates a fresh instantiation per call site (different payload types)
let inline private decEnvFields payloadDec eventCtor =
    Decode.object (fun get ->
        let id         = get.Required.Field "id"         decEventId
        let spaceId    = get.Required.Field "spaceId"    decSpaceId
        let seq        = get.Required.Field "sequence"   Decode.float |> int64
        let actorId    = get.Required.Field "actorId"    decMemberId
        let occurredAt = get.Required.Field "occurredAt" decDateTimeOffset
        let payload    = get.Required.Field "payload"    payloadDec
        eventCtor {
            Id         = id
            SpaceId    = spaceId
            Sequence   = seq
            ActorId    = actorId
            OccurredAt = occurredAt
            CreatedAt  = occurredAt
            Payload    = payload
        })

// ─── Public API ───────────────────────────────────────────────────────────────

let private mkEvent eventType envFields payloadJson =
    Encode.object [
        yield! envFields
        "eventType", Encode.string eventType
        "payload",   payloadJson
    ]

let encodeEvent (event: SpaceEvent) =
    let fields (env: EventEnvelope<_>) =
        encEnvFields env.Id env.SpaceId env.Sequence env.ActorId env.OccurredAt
    match event with
    | SpaceCreated       e -> mkEvent "SpaceCreated"       (fields e) (encSpaceCreated       e.Payload)
    | SpaceRenamed       e -> mkEvent "SpaceRenamed"       (fields e) (encSpaceRenamed       e.Payload)
    | MemberAdded        e -> mkEvent "MemberAdded"        (fields e) (encMemberAdded        e.Payload)
    | MemberRenamed      e -> mkEvent "MemberRenamed"      (fields e) (encMemberRenamed      e.Payload)
    | CategoryAdded      e -> mkEvent "CategoryAdded"      (fields e) (encCategoryAdded      e.Payload)
    | CategoryRenamed    e -> mkEvent "CategoryRenamed"    (fields e) (encCategoryRenamed    e.Payload)
    | CategoryArchived   e -> mkEvent "CategoryArchived"   (fields e) (encCategoryArchived   e.Payload)
    | ExpenseAdded       e -> mkEvent "ExpenseAdded"       (fields e) (encExpenseAdded       e.Payload)
    | ExpenseCorrected   e -> mkEvent "ExpenseCorrected"   (fields e) (encExpenseCorrected   e.Payload)
    | ExpenseDeleted     e -> mkEvent "ExpenseDeleted"     (fields e) (encExpenseDeleted     e.Payload)
    | SettlementRecorded e -> mkEvent "SettlementRecorded" (fields e) (encSettlementRecorded e.Payload)

let decodeEvent =
    Decode.field "eventType" Decode.string
    |> Decode.andThen (function
        | "SpaceCreated"       -> decEnvFields decSpaceCreated       SpaceCreated
        | "SpaceRenamed"       -> decEnvFields decSpaceRenamed       SpaceRenamed
        | "MemberAdded"        -> decEnvFields decMemberAdded        MemberAdded
        | "MemberRenamed"      -> decEnvFields decMemberRenamed      MemberRenamed
        | "CategoryAdded"      -> decEnvFields decCategoryAdded      CategoryAdded
        | "CategoryRenamed"    -> decEnvFields decCategoryRenamed    CategoryRenamed
        | "CategoryArchived"   -> decEnvFields decCategoryArchived   CategoryArchived
        | "ExpenseAdded"       -> decEnvFields decExpenseAdded       ExpenseAdded
        | "ExpenseCorrected"   -> decEnvFields decExpenseCorrected   ExpenseCorrected
        | "ExpenseDeleted"     -> decEnvFields decExpenseDeleted     ExpenseDeleted
        | "SettlementRecorded" -> decEnvFields decSettlementRecorded SettlementRecorded
        | t -> Decode.fail $"Unknown event type: {t}")

let encodeEventJson (event: SpaceEvent) : string =
    encodeEvent event |> Encode.toString 0

let decodeEventJson (json: string) : Result<SpaceEvent, string> =
    Decode.fromString decodeEvent json
