module SplitTea.Core.Tests.ValidationTests

open Xunit
open SplitTea.Core
open SplitTea.Core.Tests.Helpers

let private assertOk result =
    match result with
    | Ok _ -> ()
    | Error errs -> Assert.Fail(sprintf "Expected Ok but got Error: %A" errs)

let private assertError (expected: ValidationError) result =
    match result with
    | Ok _ -> Assert.Fail("Expected Error but got Ok")
    | Error (errs: ValidationError list) ->
        Assert.True(List.contains expected errs, sprintf "Expected %A in %A" expected errs)

[<Fact>]
let ``GroupCreated returns Ok`` () =
    assertOk (Validation.validateEvent GroupState.Empty groupCreated)

[<Fact>]
let ``MemberAdded returns Ok`` () =
    let state = Reducer.reduce GroupState.Empty groupCreated
    assertOk (Validation.validateEvent state aliceAdded)

module ``validateEvent ExpenseAdded`` =
    let private basePayload = {
        ExpenseId = expense1Id; Description = "Test"
        Amount = 10m; Currency = "GBP"; PaidBy = aliceId
        Split = Equal [aliceId; bobId]; Date = date 2024 1 1; Notes = None
    }

    let private validate payload =
        Validation.validateEvent (makeBaseState ())
            (ExpenseAdded (envelope aliceId 5 payload))

    [<Fact>]
    let ``returns Ok for valid expense`` () =
        assertOk (validate basePayload)

    [<Fact>]
    let ``AmountMustBePositive for zero amount`` () =
        assertError AmountMustBePositive (validate { basePayload with Amount = 0m })

    [<Fact>]
    let ``AmountMustBePositive for negative amount`` () =
        assertError AmountMustBePositive (validate { basePayload with Amount = -5m })

    [<Fact>]
    let ``CurrencyMismatch for wrong currency`` () =
        assertError (CurrencyMismatch ("GBP", "USD")) (validate { basePayload with Currency = "USD" })

    [<Fact>]
    let ``UnknownMember for unknown PaidBy`` () =
        assertError (UnknownMember unknownMemberId)
            (validate { basePayload with PaidBy = unknownMemberId })

    [<Fact>]
    let ``UnknownMember for unknown member in Equal split`` () =
        assertError (UnknownMember unknownMemberId)
            (validate { basePayload with Split = Equal [aliceId; unknownMemberId] })

    [<Fact>]
    let ``SplitMustHaveMembers for empty Equal split`` () =
        assertError SplitMustHaveMembers (validate { basePayload with Split = Equal [] })

    [<Fact>]
    let ``SplitMustHaveMembers for empty Exact split`` () =
        assertError SplitMustHaveMembers (validate { basePayload with Split = Exact Map.empty })

    [<Fact>]
    let ``ExactSplitSumMismatch when amounts don't sum to expense total`` () =
        let split = Exact (Map.ofList [aliceId, 3m; bobId, 3m])  // 6 != 10
        assertError (ExactSplitSumMismatch (10m, 6m))
            (validate { basePayload with Split = split })

    [<Fact>]
    let ``PercentageSumMismatch when percentages don't sum to 100`` () =
        let split = Percentage (Map.ofList [aliceId, 60m; bobId, 30m])  // 90 != 100
        assertError (PercentageSumMismatch (100m, 90m))
            (validate { basePayload with Split = split })

    [<Fact>]
    let ``SharesMustBePositive for zero share`` () =
        let split = Shares (Map.ofList [aliceId, 1; bobId, 0])
        assertError SharesMustBePositive (validate { basePayload with Split = split })

    [<Fact>]
    let ``collects multiple errors in one result`` () =
        let result = validate { basePayload with Amount = -1m; Currency = "USD" }
        assertError AmountMustBePositive result
        assertError (CurrencyMismatch ("GBP", "USD")) result

module ``validateEvent ExpenseCorrected`` =
    let private baseCorrection = {
        OriginalExpenseId = expense1Id
        Description = None; Amount = None; Currency = None
        PaidBy = None; Split = None; Date = None; Notes = None; Reason = None
    }

    let private stateWithExpense1 () =
        makeBaseState () |> fun s -> Reducer.reduce s workedExpense1

    let private validate payload =
        Validation.validateEvent (stateWithExpense1 ())
            (ExpenseCorrected (envelope aliceId 10 payload))

    [<Fact>]
    let ``returns Ok for valid correction`` () =
        assertOk (validate { baseCorrection with Description = Some "Renamed" })

    [<Fact>]
    let ``UnknownExpense for nonexistent OriginalExpenseId`` () =
        assertError (UnknownExpense unknownExpenseId)
            (validate { baseCorrection with OriginalExpenseId = unknownExpenseId })

    [<Fact>]
    let ``DeletedExpense for already-deleted expense`` () =
        let state0 = stateWithExpense1 ()
        let deletion = ExpenseDeleted (envelope aliceId 9 { ExpenseId = expense1Id; Reason = None })
        let state1 = Reducer.reduce state0 deletion
        let result =
            Validation.validateEvent state1
                (ExpenseCorrected (envelope aliceId 10 { baseCorrection with Description = Some "X" }))
        assertError (DeletedExpense expense1Id) result

    [<Fact>]
    let ``ExactSplitSumMismatch when corrected Amount breaks existing Exact split`` () =
        // Add expense with Exact split summing to 10, then correct Amount to 20
        let exactExpense = ExpenseAdded (envelope aliceId 5 {
            ExpenseId = expense3Id; Description = "Exact"
            Amount = 10m; Currency = "GBP"; PaidBy = aliceId
            Split = Exact (Map.ofList [aliceId, 5m; bobId, 5m])
            Date = date 2024 1 1; Notes = None
        })
        let state = makeBaseState () |> fun s -> Reducer.reduce s exactExpense
        let correction = ExpenseCorrected (envelope aliceId 10 {
            OriginalExpenseId = expense3Id
            Description = None; Amount = Some 20m; Currency = None
            PaidBy = None; Split = None; Date = None; Notes = None; Reason = None
        })
        assertError (ExactSplitSumMismatch (20m, 10m))
            (Validation.validateEvent state correction)

module ``validateEvent ExpenseDeleted`` =
    let private stateWithExpense1 () =
        makeBaseState () |> fun s -> Reducer.reduce s workedExpense1

    let private validate payload =
        Validation.validateEvent (stateWithExpense1 ())
            (ExpenseDeleted (envelope aliceId 10 payload))

    [<Fact>]
    let ``returns Ok for valid deletion`` () =
        assertOk (validate { ExpenseId = expense1Id; Reason = None })

    [<Fact>]
    let ``UnknownExpense for nonexistent ExpenseId`` () =
        assertError (UnknownExpense unknownExpenseId)
            (validate { ExpenseId = unknownExpenseId; Reason = None })

    [<Fact>]
    let ``DeletedExpense for already-deleted expense`` () =
        let state0 = stateWithExpense1 ()
        let deletion = ExpenseDeleted (envelope aliceId 9 { ExpenseId = expense1Id; Reason = None })
        let state1 = Reducer.reduce state0 deletion
        let result =
            Validation.validateEvent state1
                (ExpenseDeleted (envelope aliceId 10 { ExpenseId = expense1Id; Reason = None }))
        assertError (DeletedExpense expense1Id) result

module ``validateEvent SettlementRecorded`` =
    let private basePayload = {
        SettlementId = settlement1Id
        From = carolId; To = aliceId
        Amount = 42m; Currency = "GBP"
        Date = date 2024 1 3; Notes = None
    }

    let private validate payload =
        Validation.validateEvent (makeBaseState ())
            (SettlementRecorded (envelope carolId 10 payload))

    [<Fact>]
    let ``returns Ok for valid settlement`` () =
        assertOk (validate basePayload)

    [<Fact>]
    let ``SelfSettlement when From equals To`` () =
        assertError SelfSettlement (validate { basePayload with From = aliceId; To = aliceId })

    [<Fact>]
    let ``UnknownMember for unknown From`` () =
        assertError (UnknownMember unknownMemberId)
            (validate { basePayload with From = unknownMemberId })

    [<Fact>]
    let ``UnknownMember for unknown To`` () =
        assertError (UnknownMember unknownMemberId)
            (validate { basePayload with To = unknownMemberId })

    [<Fact>]
    let ``AmountMustBePositive for zero amount`` () =
        assertError AmountMustBePositive (validate { basePayload with Amount = 0m })

    [<Fact>]
    let ``CurrencyMismatch for wrong currency`` () =
        assertError (CurrencyMismatch ("GBP", "EUR"))
            (validate { basePayload with Currency = "EUR" })
