module SplitTea.Core.Tests.Helpers

open SplitTea.Core

let groupId = GroupId  (System.Guid.Parse "00000000-0000-0000-0000-000000000010")
let aliceId = MemberId (System.Guid.Parse "00000000-0000-0000-0000-000000000001")
let bobId   = MemberId (System.Guid.Parse "00000000-0000-0000-0000-000000000002")
let carolId = MemberId (System.Guid.Parse "00000000-0000-0000-0000-000000000003")

let unknownMemberId  = MemberId  (System.Guid.Parse "ffffffff-0000-0000-0000-000000000001")
let unknownExpenseId = ExpenseId (System.Guid.Parse "ffffffff-0000-0000-0000-000000000002")

let alice : Member = { Id = aliceId; DisplayName = "Alice"; UserId = None }
let bob   : Member = { Id = bobId;   DisplayName = "Bob";   UserId = None }
let carol : Member = { Id = carolId; DisplayName = "Carol"; UserId = None }

let expense1Id    = ExpenseId    (System.Guid.Parse "00000000-0000-0000-0001-000000000001")
let expense2Id    = ExpenseId    (System.Guid.Parse "00000000-0000-0000-0001-000000000002")
let expense3Id    = ExpenseId    (System.Guid.Parse "00000000-0000-0000-0001-000000000003")
let settlement1Id = SettlementId (System.Guid.Parse "00000000-0000-0000-0002-000000000001")
let settlement2Id = SettlementId (System.Guid.Parse "00000000-0000-0000-0002-000000000002")

let envelope actorId seq payload : EventEnvelope<_> = {
    Id         = EventId System.Guid.Empty
    GroupId    = groupId
    Sequence   = int64 seq
    ActorId    = actorId
    OccurredAt = System.DateTimeOffset.UnixEpoch
    CreatedAt  = System.DateTimeOffset.UnixEpoch
    Payload    = payload
}

let date y m d = System.DateOnly(y, m, d)

let groupCreated = GroupCreated (envelope aliceId 1 {
    Name = "Trip"; Currency = "GBP"; CreatedBy = aliceId
})

let aliceAdded = MemberAdded (envelope aliceId 2 { Member = alice })
let bobAdded   = MemberAdded (envelope aliceId 3 { Member = bob })
let carolAdded = MemberAdded (envelope aliceId 4 { Member = carol })

// Worked example:
//   Expense 1 — Alice pays £84, split equally [Alice, Carol] → Carol owes Alice £42
//   Expense 2 — Bob pays £164, split equally [Bob, Carol]   → Carol owes Bob  £82
//   Expected net positions: Alice +42, Bob +82, Carol -124

let workedExpense1 = ExpenseAdded (envelope aliceId 5 {
    ExpenseId = expense1Id; Description = "Dinner"
    PaidAmount = 84m; PaidCurrency = "GBP"; ExchangeRate = None; PaidBy = aliceId
    Split = Equal [aliceId; carolId]; Date = date 2024 1 1; Category = None; Notes = None; ContextId = None
})

let workedExpense2 = ExpenseAdded (envelope bobId 6 {
    ExpenseId = expense2Id; Description = "Hotel"
    PaidAmount = 164m; PaidCurrency = "GBP"; ExchangeRate = None; PaidBy = bobId
    Split = Equal [bobId; carolId]; Date = date 2024 1 2; Category = None; Notes = None; ContextId = None
})

let makeBaseState () =
    Reducer.replayEvents [ groupCreated; aliceAdded; bobAdded; carolAdded ]

let makeWorkedState () =
    Reducer.replayEvents [
        groupCreated; aliceAdded; bobAdded; carolAdded
        workedExpense1; workedExpense2
    ]

let findPosition memberId positions =
    positions |> List.find (fun (p: NetPosition) -> p.MemberId = memberId)
