namespace SplitTea.Core

type GroupId      = GroupId      of System.Guid
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

type Split =
    | Equal      of members: MemberId list
    | Exact      of shares: Map<MemberId, Amount>
    | Percentage of shares: Map<MemberId, decimal>
    | Shares     of shares: Map<MemberId, int>
