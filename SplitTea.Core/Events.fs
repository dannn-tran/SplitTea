namespace SplitTea.Core

type EventEnvelope<'Payload> = {
    Id: EventId
    SpaceId: SpaceId
    Sequence: int64
    ActorId: MemberId
    OccurredAt: System.DateTimeOffset
    Payload: 'Payload
}

type SpaceCreatedPayload = {
    Name       : string
    Currency   : CurrencyCode
    CreatedBy  : MemberId
    Categories : string list
}

type MemberAddedPayload = {
    Member: Member
}

type CategoryAddedPayload = {
    Name: string
}

type CategoryRenamedPayload = {
    OldName: string
    NewName: string
}

type CategoryArchivedPayload = {
    Name: string
}

type SpaceRenamedPayload = {
    NewName: string
}

type MemberRenamedPayload = {
    MemberId: MemberId
    NewName: string
}

type ExpenseAddedPayload = {
    ExpenseId    : ExpenseId
    Description  : string
    PaidAmount   : Amount
    PaidCurrency : CurrencyCode
    PaidBy       : MemberId
    Split        : Split
    Date         : System.DateOnly
    Category     : string option
    Notes        : string option
}

type ExpenseCorrectedPayload = {
    OriginalExpenseId : ExpenseId
    Description       : string option
    PaidAmount        : Amount option
    PaidCurrency      : CurrencyCode option
    PaidBy            : MemberId option
    Split             : Split option
    Date              : System.DateOnly option
    Category          : string Patch
    Notes             : string Patch
    Reason            : string option
}

type ExpenseDeletedPayload = {
    ExpenseId: ExpenseId
    Reason: string option
}

type SettlementLeg = {
    Amount   : Amount
    Currency : CurrencyCode
}

type SettlementRecordedPayload = {
    SettlementId : SettlementId
    From         : MemberId
    To           : MemberId
    Payments     : SettlementLeg list
    Date         : System.DateOnly
    Notes        : string option
}

type SpaceEvent =
    | SpaceCreated       of EventEnvelope<SpaceCreatedPayload>
    | SpaceRenamed       of EventEnvelope<SpaceRenamedPayload>
    | MemberAdded        of EventEnvelope<MemberAddedPayload>
    | MemberRenamed      of EventEnvelope<MemberRenamedPayload>
    | CategoryAdded      of EventEnvelope<CategoryAddedPayload>
    | CategoryRenamed    of EventEnvelope<CategoryRenamedPayload>
    | CategoryArchived   of EventEnvelope<CategoryArchivedPayload>
    | ExpenseAdded       of EventEnvelope<ExpenseAddedPayload>
    | ExpenseCorrected   of EventEnvelope<ExpenseCorrectedPayload>
    | ExpenseDeleted     of EventEnvelope<ExpenseDeletedPayload>
    | SettlementRecorded of EventEnvelope<SettlementRecordedPayload>

module SpaceEvent =
    let getId = function
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

    let getSpaceId = function
        | SpaceCreated e       -> e.SpaceId
        | SpaceRenamed e       -> e.SpaceId
        | MemberAdded e        -> e.SpaceId
        | MemberRenamed e      -> e.SpaceId
        | CategoryAdded e      -> e.SpaceId
        | CategoryRenamed e    -> e.SpaceId
        | CategoryArchived e   -> e.SpaceId
        | ExpenseAdded e       -> e.SpaceId
        | ExpenseCorrected e   -> e.SpaceId
        | ExpenseDeleted e     -> e.SpaceId
        | SettlementRecorded e -> e.SpaceId

    let getActorId = function
        | SpaceCreated e       -> e.ActorId
        | SpaceRenamed e       -> e.ActorId
        | MemberAdded e        -> e.ActorId
        | MemberRenamed e      -> e.ActorId
        | CategoryAdded e      -> e.ActorId
        | CategoryRenamed e    -> e.ActorId
        | CategoryArchived e   -> e.ActorId
        | ExpenseAdded e       -> e.ActorId
        | ExpenseCorrected e   -> e.ActorId
        | ExpenseDeleted e     -> e.ActorId
        | SettlementRecorded e -> e.ActorId

    let withActorId (actor: MemberId) ev =
        let stamp (env: EventEnvelope<'p>) = { env with ActorId = actor }
        match ev with
        | SpaceCreated e       -> SpaceCreated       (stamp e)
        | SpaceRenamed e       -> SpaceRenamed       (stamp e)
        | MemberAdded e        -> MemberAdded        (stamp e)
        | MemberRenamed e      -> MemberRenamed      (stamp e)
        | CategoryAdded e      -> CategoryAdded      (stamp e)
        | CategoryRenamed e    -> CategoryRenamed    (stamp e)
        | CategoryArchived e   -> CategoryArchived   (stamp e)
        | ExpenseAdded e       -> ExpenseAdded       (stamp e)
        | ExpenseCorrected e   -> ExpenseCorrected   (stamp e)
        | ExpenseDeleted e     -> ExpenseDeleted     (stamp e)
        | SettlementRecorded e -> SettlementRecorded (stamp e)
