namespace SplitTea.Core

type GroupId      = GroupId      of System.Guid
type MemberId     = MemberId     of System.Guid
type ExpenseId    = ExpenseId    of System.Guid
type SettlementId = SettlementId of System.Guid
type ContextId    = ContextId    of System.Guid
type EventId      = EventId      of System.Guid
type UserId       = UserId       of System.Guid

type CurrencyCode = string
type Amount       = decimal

type Member = {
    Id: MemberId
    DisplayName: string
    UserId: UserId option
}

type Split =
    | Equal      of members: MemberId list
    | Exact      of shares: Map<MemberId, Amount>
    | Percentage of shares: Map<MemberId, decimal>
    | Shares     of shares: Map<MemberId, int>

type ContextTemplate =
    | Trip      // user-named, optional date bounds
    | Monthly   // calendar month — "January 2025"
    | Weekly    // calendar week — "Week 3 · Jan 15–21"
    | Custom    // user-defined date range

/// Used in correction events to distinguish "leave unchanged" from "explicitly clear to None".
type Patch<'a> =
    | Unchanged
    | Clear
    | SetTo of 'a
