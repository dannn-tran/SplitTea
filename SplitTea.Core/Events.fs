namespace SplitTea.Core

type EventEnvelope<'Payload> = {
    Id: EventId
    SpaceId: SpaceId
    Sequence: int64
    ActorId: MemberId
    OccurredAt: System.DateTimeOffset
    CreatedAt: System.DateTimeOffset
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
    ExchangeRate : decimal option  // PaidCurrency → group currency; None when same currency
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
    ExchangeRate      : decimal Patch
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

type SettlementRecordedPayload = {
    SettlementId : SettlementId
    From         : MemberId
    To           : MemberId
    Amount       : Amount
    Currency     : CurrencyCode
    ExchangeRate : decimal option  // Currency → group currency; None when same currency
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
