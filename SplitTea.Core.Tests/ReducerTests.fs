module SplitTea.Core.Tests.ReducerTests

open Xunit
open SplitTea.Core
open SplitTea.Core.Tests.Helpers

[<Fact>]
let ``replayEvents empty list returns GroupState.Empty`` () =
    Assert.Equal(GroupState.Empty, Reducer.replayEvents [])

module ``reduce GroupCreated`` =
    [<Fact>]
    let ``sets GroupId Name Currency`` () =
        let state = Reducer.reduce GroupState.Empty groupCreated
        Assert.Equal(groupId, state.GroupId)
        Assert.Equal("Trip", state.Name)
        Assert.Equal("GBP", state.Currency)

    [<Fact>]
    let ``Members and Expenses and Settlements are empty`` () =
        let state = Reducer.reduce GroupState.Empty groupCreated
        Assert.Equal(0, Map.count state.Members)
        Assert.Equal(0, Map.count state.Expenses)
        Assert.Empty(state.Settlements)

    [<Fact>]
    let ``subsequent GroupCreated resets state`` () =
        let state0 = makeBaseState ()
        let reset  = GroupCreated (envelope aliceId 99 { Name = "New"; Currency = "EUR"; CreatedBy = aliceId })
        let state1 = Reducer.reduce state0 reset
        Assert.Equal(0, Map.count state1.Members)
        Assert.Equal("New", state1.Name)

module ``reduce MemberAdded`` =
    [<Fact>]
    let ``adds member to Members map`` () =
        let state0 = Reducer.reduce GroupState.Empty groupCreated
        let state1 = Reducer.reduce state0 aliceAdded
        Assert.True(Map.containsKey aliceId state1.Members)
        Assert.Equal(alice, Map.find aliceId state1.Members)

    [<Fact>]
    let ``accumulates multiple members`` () =
        let state = makeBaseState ()
        Assert.Equal(3, Map.count state.Members)

module ``reduce ExpenseAdded`` =
    [<Fact>]
    let ``adds expense with IsDeleted false`` () =
        let state = makeBaseState () |> fun s -> Reducer.reduce s workedExpense1
        let exp   = Map.find expense1Id state.Expenses
        Assert.False(exp.IsDeleted)
        Assert.Equal(84m, exp.PaidAmount)
        Assert.Equal(aliceId, exp.PaidBy)

    [<Fact>]
    let ``does not affect other data`` () =
        let state = makeBaseState () |> fun s -> Reducer.reduce s workedExpense1
        Assert.Equal(3, Map.count state.Members)
        Assert.Equal(1, Map.count state.Expenses)

module ``reduce ExpenseCorrected`` =
    let private baseWithExpense () =
        makeBaseState () |> fun s -> Reducer.reduce s workedExpense1

    let private correctWith actorId payload =
        ExpenseCorrected (envelope actorId 10 payload)

    [<Fact>]
    let ``merges Some fields, leaves None unchanged`` () =
        let state0 = baseWithExpense ()
        let correction = correctWith aliceId {
            OriginalExpenseId = expense1Id
            Description = Some "Updated"; PaidAmount = None; PaidCurrency = None; ExchangeRate = Unchanged
            PaidBy = None; Split = None; Date = None; Category = Unchanged; Notes = Unchanged; ContextId = Unchanged; Reason = None
        }
        let exp = Reducer.reduce state0 correction |> fun s -> Map.find expense1Id s.Expenses
        Assert.Equal("Updated", exp.Description)
        Assert.Equal(84m, exp.PaidAmount)

    [<Fact>]
    let ``Notes = Clear clears notes`` () =
        let withNotes = ExpenseAdded (envelope aliceId 5 {
            ExpenseId = expense3Id; Description = "X"
            PaidAmount = 10m; PaidCurrency = "GBP"; ExchangeRate = None; PaidBy = aliceId
            Split = Equal [aliceId]; Date = date 2024 1 1; Category = None; Notes = Some "keep"; ContextId = None
        })
        let clear = ExpenseCorrected (envelope aliceId 10 {
            OriginalExpenseId = expense3Id
            Description = None; PaidAmount = None; PaidCurrency = None; ExchangeRate = Unchanged
            PaidBy = None; Split = None; Date = None
            Category = Unchanged; Notes = Clear; ContextId = Unchanged
            Reason = None
        })
        let state =
            makeBaseState ()
            |> fun s -> Reducer.reduce s withNotes
            |> fun s -> Reducer.reduce s clear
        Assert.Equal(None, (Map.find expense3Id state.Expenses).Notes)

    [<Fact>]
    let ``Notes = Unchanged leaves notes unchanged`` () =
        let withNotes = ExpenseAdded (envelope aliceId 5 {
            ExpenseId = expense3Id; Description = "X"
            PaidAmount = 10m; PaidCurrency = "GBP"; ExchangeRate = None; PaidBy = aliceId
            Split = Equal [aliceId]; Date = date 2024 1 1; Category = None; Notes = Some "keep"; ContextId = None
        })
        let noChange = ExpenseCorrected (envelope aliceId 10 {
            OriginalExpenseId = expense3Id
            Description = None; PaidAmount = None; PaidCurrency = None; ExchangeRate = Unchanged
            PaidBy = None; Split = None; Date = None
            Category = Unchanged; Notes = Unchanged; ContextId = Unchanged
            Reason = None
        })
        let state =
            makeBaseState ()
            |> fun s -> Reducer.reduce s withNotes
            |> fun s -> Reducer.reduce s noChange
        Assert.Equal(Some "keep", (Map.find expense3Id state.Expenses).Notes)

    [<Fact>]
    let ``unknown OriginalExpenseId is no-op`` () =
        let state0 = baseWithExpense ()
        let correction = correctWith aliceId {
            OriginalExpenseId = unknownExpenseId
            Description = Some "X"; PaidAmount = None; PaidCurrency = None; ExchangeRate = Unchanged
            PaidBy = None; Split = None; Date = None; Category = Unchanged; Notes = Unchanged; ContextId = Unchanged; Reason = None
        }
        let state1 = Reducer.reduce state0 correction
        Assert.True(state0.Expenses = state1.Expenses)

module ``reduce ExpenseDeleted`` =
    [<Fact>]
    let ``sets IsDeleted true`` () =
        let state0 = makeBaseState () |> fun s -> Reducer.reduce s workedExpense1
        let deletion = ExpenseDeleted (envelope aliceId 10 { ExpenseId = expense1Id; Reason = None })
        let state1 = Reducer.reduce state0 deletion
        Assert.True((Map.find expense1Id state1.Expenses).IsDeleted)

    [<Fact>]
    let ``unknown ExpenseId is no-op`` () =
        let state0 = makeBaseState ()
        let deletion = ExpenseDeleted (envelope aliceId 10 { ExpenseId = unknownExpenseId; Reason = None })
        let state1 = Reducer.reduce state0 deletion
        Assert.True(state0.Expenses = state1.Expenses)

module ``reduce SettlementRecorded`` =
    [<Fact>]
    let ``appends payload to Settlements`` () =
        let s = SettlementRecorded (envelope carolId 10 {
            SettlementId = settlement1Id; From = carolId; To = aliceId
            Amount = 42m; Currency = "GBP"; ExchangeRate = None; Date = date 2024 1 3; Notes = None
        })
        let state = makeBaseState () |> fun st -> Reducer.reduce st s
        Assert.Equal(1, List.length state.Settlements)
        Assert.Equal(carolId, state.Settlements.[0].From)
        Assert.Equal(aliceId, state.Settlements.[0].To)

    [<Fact>]
    let ``accumulates multiple settlements`` () =
        let s1 = SettlementRecorded (envelope carolId 10 {
            SettlementId = settlement1Id; From = carolId; To = aliceId
            Amount = 42m; Currency = "GBP"; ExchangeRate = None; Date = date 2024 1 3; Notes = None
        })
        let s2 = SettlementRecorded (envelope carolId 11 {
            SettlementId = settlement2Id; From = carolId; To = bobId
            Amount = 82m; Currency = "GBP"; ExchangeRate = None; Date = date 2024 1 3; Notes = None
        })
        let state =
            makeBaseState ()
            |> fun st -> Reducer.reduce st s1
            |> fun st -> Reducer.reduce st s2
        Assert.Equal(2, List.length state.Settlements)
