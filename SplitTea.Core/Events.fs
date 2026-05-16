namespace SplitTea.Core

type EventEnvelope<'Payload> = {
    Id: EventId
    GroupId: GroupId
    Sequence: int64
    ActorId: MemberId
    OccurredAt: System.DateTimeOffset
    CreatedAt: System.DateTimeOffset
    Payload: 'Payload
}

type GroupCreatedPayload = {
    Name: string
    Currency: CurrencyCode
    CreatedBy: MemberId
}

type MemberAddedPayload = {
    Member: Member
}

type ContextCreatedPayload = {
    ContextId : ContextId
    Name      : string
    Template  : ContextTemplate
    Members   : MemberId list option  // None = all group members
    DateFrom  : System.DateOnly option
    DateTo    : System.DateOnly option
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
    ContextId    : ContextId option
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
    ContextId         : ContextId Patch  // Clear = remove from context; SetTo = assign
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

type GroupEvent =
    | GroupCreated       of EventEnvelope<GroupCreatedPayload>
    | MemberAdded        of EventEnvelope<MemberAddedPayload>
    | ContextCreated     of EventEnvelope<ContextCreatedPayload>
    | ExpenseAdded       of EventEnvelope<ExpenseAddedPayload>
    | ExpenseCorrected   of EventEnvelope<ExpenseCorrectedPayload>
    | ExpenseDeleted     of EventEnvelope<ExpenseDeletedPayload>
    | SettlementRecorded of EventEnvelope<SettlementRecordedPayload>
