namespace SplitTea.Core

type SpaceId      = SpaceId      of System.Guid
type MemberId     = MemberId     of System.Guid
type ExpenseId    = ExpenseId    of System.Guid
type SettlementId = SettlementId of System.Guid
type EventId      = EventId      of System.Guid
type UserId       = UserId       of System.Guid

type CurrencyCode = string
type Amount       = decimal

type Member = {
    Id: MemberId
    DisplayName: string
    UserId: UserId option
}

type CategoryState = {
    Name       : string
    IsArchived : bool
}

type Split =
    | Equal      of members: MemberId list
    | Exact      of shares: Map<MemberId, Amount>
    | Percentage of shares: Map<MemberId, decimal>
    | Shares     of shares: Map<MemberId, int>

/// Used in correction events to distinguish "leave unchanged" from "explicitly clear to None".
type Patch<'a> =
    | Unchanged
    | Clear
    | SetTo of 'a
