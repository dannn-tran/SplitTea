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

type ExpenseAddedPayload = {
    ExpenseId: ExpenseId
    Description: string
    Amount: Amount
    Currency: CurrencyCode
    PaidBy: MemberId
    Split: Split
    Date: System.DateOnly
    Notes: string option
}

type ExpenseCorrectedPayload = {
    OriginalExpenseId: ExpenseId
    Description: string option
    Amount: Amount option
    Currency: CurrencyCode option
    PaidBy: MemberId option
    Split: Split option
    Date: System.DateOnly option
    Notes: string option option  // Some None = clear; None = unchanged
    Reason: string option
}

type ExpenseDeletedPayload = {
    ExpenseId: ExpenseId
    Reason: string option
}

type SettlementRecordedPayload = {
    SettlementId: SettlementId
    From: MemberId
    To: MemberId
    Amount: Amount
    Currency: CurrencyCode
    Date: System.DateOnly
    Notes: string option
}

type GroupEvent =
    | GroupCreated       of EventEnvelope<GroupCreatedPayload>
    | MemberAdded        of EventEnvelope<MemberAddedPayload>
    | ExpenseAdded       of EventEnvelope<ExpenseAddedPayload>
    | ExpenseCorrected   of EventEnvelope<ExpenseCorrectedPayload>
    | ExpenseDeleted     of EventEnvelope<ExpenseDeletedPayload>
    | SettlementRecorded of EventEnvelope<SettlementRecordedPayload>
