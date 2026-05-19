module SplitTea.Core.Tests.RebaseTests

open Xunit
open SplitTea.Core
open SplitTea.Core.Tests.Helpers

let private syncedBase = [ spaceCreated; aliceAdded; bobAdded; carolAdded ]

let private expenseByAlice = ExpenseAdded (envelope aliceId 10 {
    ExpenseId = expense1Id; Description = "Dinner"
    PaidAmount = 50m; PaidCurrency = "GBP"; PaidBy = aliceId
    Split = Equal [aliceId; bobId]; Date = date 2024 6 1; Category = None; Notes = None
})

let private correctionByAlice = ExpenseCorrected (envelope aliceId 11 {
    OriginalExpenseId = expense1Id
    Description = Some "Dinner (corrected)"; PaidAmount = None; PaidCurrency = None
    PaidBy = None; Split = None; Date = None
    Category = Unchanged; Notes = Unchanged; Reason = None
})

let private deletionByAlice = ExpenseDeleted (envelope aliceId 12 {
    ExpenseId = expense1Id; Reason = None
})

module ``Rebase apply`` =

    [<Fact>]
    let ``no pending events returns server state and no conflicts`` () =
        let (state, conflicts) = Rebase.apply syncedBase []
        Assert.Equal("Trip", state.Name)
        Assert.Empty(conflicts)

    [<Fact>]
    let ``valid pending event included in display state`` () =
        let (state, conflicts) = Rebase.apply syncedBase [expenseByAlice]
        Assert.True(Map.containsKey expense1Id state.Expenses)
        Assert.Empty(conflicts)

    [<Fact>]
    let ``multiple valid pending events all included`` () =
        let pending = [ expenseByAlice; ExpenseAdded (envelope bobId 11 {
            ExpenseId = expense2Id; Description = "Hotel"
            PaidAmount = 120m; PaidCurrency = "GBP"; PaidBy = bobId
            Split = Equal [bobId; carolId]; Date = date 2024 6 2; Category = None; Notes = None
        }) ]
        let (state, conflicts) = Rebase.apply syncedBase pending
        Assert.True(Map.containsKey expense1Id state.Expenses)
        Assert.True(Map.containsKey expense2Id state.Expenses)
        Assert.Empty(conflicts)

    [<Fact>]
    let ``pending correction of server-deleted expense is conflicted`` () =
        let serverDeletes = syncedBase @ [ expenseByAlice; deletionByAlice ]
        let (state, conflicts) = Rebase.apply serverDeletes [correctionByAlice]
        // correction was not applied: expense remains deleted with original description
        Assert.True(state.Expenses[expense1Id].IsDeleted)
        Assert.Equal("Dinner", state.Expenses[expense1Id].Description)
        Assert.Single(conflicts) |> ignore
        Assert.Equal(correctionByAlice, conflicts[0].Event)
        Assert.Contains(DeletedExpense expense1Id, conflicts[0].Errors)

    [<Fact>]
    let ``pending deletion of server-deleted expense is conflicted`` () =
        let serverDeletes = syncedBase @ [ expenseByAlice; deletionByAlice ]
        let (state, conflicts) = Rebase.apply serverDeletes [deletionByAlice]
        Assert.True(state.Expenses[expense1Id].IsDeleted)
        Assert.Single(conflicts) |> ignore
        Assert.Contains(DeletedExpense expense1Id, conflicts[0].Errors)

    [<Fact>]
    let ``cascading: correction depends on pending add; add conflicts; correction also conflicts`` () =
        // serverBase has no expense1Id; pending add by unknown actor (conflicts);
        // pending correction of expense1Id (should also conflict since add failed)
        let pendingAdd = ExpenseAdded (envelope unknownMemberId 10 {
            ExpenseId = expense1Id; Description = "Dinner"
            PaidAmount = 50m; PaidCurrency = "GBP"; PaidBy = aliceId
            Split = Equal [aliceId; bobId]; Date = date 2024 6 1; Category = None; Notes = None
        })
        let (state, conflicts) = Rebase.apply syncedBase [pendingAdd; correctionByAlice]
        Assert.Equal(2, List.length conflicts)
        Assert.Equal(pendingAdd, conflicts[0].Event)
        Assert.Contains(ActorNotMember unknownMemberId, conflicts[0].Errors)
        Assert.Equal(correctionByAlice, conflicts[1].Event)
        Assert.Contains(UnknownExpense expense1Id, conflicts[1].Errors)
        Assert.False(Map.containsKey expense1Id state.Expenses)

    [<Fact>]
    let ``conflicted event does not affect accumulated state for subsequent valid events`` () =
        // invalid event (unknown actor), then valid expense by bob — bob's should appear
        let invalidEvent = ExpenseAdded (envelope unknownMemberId 10 {
            ExpenseId = expense1Id; Description = "Invalid"
            PaidAmount = 50m; PaidCurrency = "GBP"; PaidBy = aliceId
            Split = Equal [aliceId]; Date = date 2024 6 1; Category = None; Notes = None
        })
        let validEvent = ExpenseAdded (envelope bobId 11 {
            ExpenseId = expense2Id; Description = "Valid by Bob"
            PaidAmount = 30m; PaidCurrency = "GBP"; PaidBy = bobId
            Split = Equal [bobId]; Date = date 2024 6 2; Category = None; Notes = None
        })
        let (state, conflicts) = Rebase.apply syncedBase [invalidEvent; validEvent]
        Assert.Single(conflicts) |> ignore
        Assert.Equal(invalidEvent, conflicts[0].Event)
        Assert.False(Map.containsKey expense1Id state.Expenses)
        Assert.True(Map.containsKey expense2Id state.Expenses)

    [<Fact>]
    let ``unrelated server event does not conflict valid pending`` () =
        // server renames space; pending expense by alice should still be valid
        let serverWithRename = syncedBase @ [ SpaceRenamed (envelope aliceId 10 { NewName = "Euro Trip" }) ]
        let (state, conflicts) = Rebase.apply serverWithRename [expenseByAlice]
        Assert.Empty(conflicts)
        Assert.True(Map.containsKey expense1Id state.Expenses)
        Assert.Equal("Euro Trip", state.Name)

    [<Fact>]
    let ``result equals replayEvents when all pending are valid`` () =
        let allEvents = syncedBase @ [expenseByAlice]
        let expected = Reducer.replayEvents allEvents
        let (actual, conflicts) = Rebase.apply syncedBase [expenseByAlice]
        Assert.Empty(conflicts)
        Assert.Equal(expected, actual)
