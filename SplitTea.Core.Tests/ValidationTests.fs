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
let ``SpaceCreated returns Ok`` () =
    assertOk (Validation.validateEvent SpaceState.Empty spaceCreated)

module ``validateEvent MemberAdded`` =
    let private baseState () = Reducer.reduce SpaceState.Empty spaceCreated

    [<Fact>]
    let ``returns Ok for a new member`` () =
        assertOk (Validation.validateEvent (baseState ()) aliceAdded)

    [<Fact>]
    let ``DuplicateMember when member ID already exists`` () =
        let state = Reducer.reduce (baseState ()) aliceAdded
        assertError (DuplicateMember aliceId) (Validation.validateEvent state aliceAdded)

module ``validateEvent ExpenseAdded`` =
    let private basePayload : ExpenseAddedPayload = {
        ExpenseId = expense1Id; Description = "Test"
        PaidAmount = 10m; PaidCurrency = "GBP"; ExchangeRate = None; PaidBy = aliceId
        Split = Equal [aliceId; bobId]; Date = date 2024 1 1; Category = None; Notes = None
    }

    let private validate payload =
        Validation.validateEvent (makeBaseState ())
            (ExpenseAdded (envelope aliceId 5 payload))

    [<Fact>]
    let ``returns Ok for valid expense`` () =
        assertOk (validate basePayload)

    [<Fact>]
    let ``ActorNotMember for unknown actor`` () =
        Validation.validateEvent (makeBaseState ())
            (ExpenseAdded (envelope unknownMemberId 5 basePayload))
        |> assertError (ActorNotMember unknownMemberId)

    [<Fact>]
    let ``AmountMustBePositive for zero amount`` () =
        assertError AmountMustBePositive (validate { basePayload with PaidAmount = 0m })

    [<Fact>]
    let ``AmountMustBePositive for negative amount`` () =
        assertError AmountMustBePositive (validate { basePayload with PaidAmount = -5m })

    [<Fact>]
    let ``foreign currency expense is valid`` () =
        assertOk (validate { basePayload with PaidCurrency = "USD"; ExchangeRate = Some 0.79m })

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
        assertError SplitMustHaveMembers (validate { basePayload with Split = Exact [] })

    [<Fact>]
    let ``ExactSplitSumMismatch when amounts don't sum to expense total`` () =
        let split = Exact [aliceId, 3m; bobId, 3m]  // 6 != 10
        assertError (ExactSplitSumMismatch (10m, 6m))
            (validate { basePayload with Split = split })

    [<Fact>]
    let ``PercentageSumMismatch when percentages don't sum to 100`` () =
        let split = Percentage [aliceId, 60m; bobId, 30m]  // 90 != 100
        assertError (PercentageSumMismatch (100m, 90m))
            (validate { basePayload with Split = split })

    [<Fact>]
    let ``SharesMustBePositive for zero share`` () =
        let split = Shares [aliceId, 1; bobId, 0]
        assertError SharesMustBePositive (validate { basePayload with Split = split })

    [<Fact>]
    let ``collects multiple errors in one result`` () =
        let result = validate { basePayload with PaidAmount = -1m; PaidBy = unknownMemberId }
        assertError AmountMustBePositive result
        assertError (UnknownMember unknownMemberId) result

module ``validateEvent ExpenseCorrected`` =
    let private baseCorrection : ExpenseCorrectedPayload = {
        OriginalExpenseId = expense1Id
        Description = None; PaidAmount = None; PaidCurrency = None; ExchangeRate = Unchanged
        PaidBy = None; Split = None; Date = None; Category = Unchanged; Notes = Unchanged; Reason = None
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
    let ``ActorNotMember for unknown actor`` () =
        Validation.validateEvent (stateWithExpense1 ())
            (ExpenseCorrected (envelope unknownMemberId 10 { baseCorrection with Description = Some "X" }))
        |> assertError (ActorNotMember unknownMemberId)

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
    let ``ExactSplitSumMismatch when corrected PaidAmount breaks existing Exact split`` () =
        // Add expense with Exact split summing to 10, then correct PaidAmount to 20
        let exactExpense = ExpenseAdded (envelope aliceId 5 {
            ExpenseId = expense3Id; Description = "Exact"
            PaidAmount = 10m; PaidCurrency = "GBP"; ExchangeRate = None; PaidBy = aliceId
            Split = Exact [aliceId, 5m; bobId, 5m]
            Date = date 2024 1 1; Category = None; Notes = None
        })
        let state = makeBaseState () |> fun s -> Reducer.reduce s exactExpense
        let correction = ExpenseCorrected (envelope aliceId 10 {
            OriginalExpenseId = expense3Id
            Description = None; PaidAmount = Some 20m; PaidCurrency = None; ExchangeRate = Unchanged
            PaidBy = None; Split = None; Date = None; Category = Unchanged; Notes = Unchanged; Reason = None
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
    let ``ActorNotMember for unknown actor`` () =
        Validation.validateEvent (stateWithExpense1 ())
            (ExpenseDeleted (envelope unknownMemberId 10 { ExpenseId = expense1Id; Reason = None }))
        |> assertError (ActorNotMember unknownMemberId)

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
    let private basePayload : SettlementRecordedPayload = {
        SettlementId = settlement1Id
        From = carolId; To = aliceId
        Amount = 42m; Currency = "GBP"; ExchangeRate = None
        Date = date 2024 1 3; Notes = None
    }

    let private validate payload =
        Validation.validateEvent (makeBaseState ())
            (SettlementRecorded (envelope carolId 10 payload))

    [<Fact>]
    let ``returns Ok for valid settlement`` () =
        assertOk (validate basePayload)

    [<Fact>]
    let ``ActorNotMember for unknown actor`` () =
        Validation.validateEvent (makeBaseState ())
            (SettlementRecorded (envelope unknownMemberId 10 basePayload))
        |> assertError (ActorNotMember unknownMemberId)

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
    let ``foreign currency settlement is valid`` () =
        assertOk (validate { basePayload with Currency = "EUR"; ExchangeRate = Some 1.17m })

module ``validateEvent MemberRenamed`` =
    let private validate (actorId: MemberId) (memberId: MemberId) =
        Validation.validateEvent (makeBaseState ())
            (MemberRenamed (envelope actorId 10 { MemberId = memberId; NewName = "New" }))

    [<Fact>]
    let ``returns Ok when actor renames themselves`` () =
        assertOk (validate aliceId aliceId)

    [<Fact>]
    let ``CannotRenameOtherMember when actor renames a different member`` () =
        assertError CannotRenameOtherMember (validate aliceId bobId)

    [<Fact>]
    let ``UnknownMember when memberId not in space`` () =
        assertError (UnknownMember unknownMemberId) (validate aliceId unknownMemberId)

    [<Fact>]
    let ``ActorNotMember when actorId not in space`` () =
        assertError (ActorNotMember unknownMemberId) (validate unknownMemberId aliceId)

module ``validateEvent SpaceRenamed`` =
    let private validate (actorId: MemberId) =
        Validation.validateEvent (makeBaseState ())
            (SpaceRenamed (envelope actorId 10 { NewName = "New Name" }))

    [<Fact>]
    let ``returns Ok for known actor`` () =
        assertOk (validate aliceId)

    [<Fact>]
    let ``ActorNotMember for unknown actor`` () =
        assertError (ActorNotMember unknownMemberId) (validate unknownMemberId)

module ``validateEvent CategoryAdded`` =
    let private validate (actorId: MemberId) =
        Validation.validateEvent (makeBaseState ())
            (CategoryAdded (envelope actorId 10 { Name = "Misc" }))

    [<Fact>]
    let ``returns Ok for known actor`` () =
        assertOk (validate aliceId)

    [<Fact>]
    let ``ActorNotMember for unknown actor`` () =
        assertError (ActorNotMember unknownMemberId) (validate unknownMemberId)

module ``validateEvent CategoryRenamed`` =
    let private baseState () =
        makeBaseState () |> fun s -> Reducer.reduce s (CategoryAdded (envelope aliceId 5 { Name = "Old" }))

    let private validate (actorId: MemberId) =
        Validation.validateEvent (baseState ())
            (CategoryRenamed (envelope actorId 10 { OldName = "Old"; NewName = "New" }))

    [<Fact>]
    let ``returns Ok for known actor`` () =
        assertOk (validate aliceId)

    [<Fact>]
    let ``ActorNotMember for unknown actor`` () =
        assertError (ActorNotMember unknownMemberId) (validate unknownMemberId)

module ``validateEvent CategoryArchived`` =
    let private baseState () =
        makeBaseState () |> fun s -> Reducer.reduce s (CategoryAdded (envelope aliceId 5 { Name = "Food" }))

    let private validate (actorId: MemberId) =
        Validation.validateEvent (baseState ())
            (CategoryArchived (envelope actorId 10 { Name = "Food" }))

    [<Fact>]
    let ``returns Ok for known actor`` () =
        assertOk (validate aliceId)

    [<Fact>]
    let ``ActorNotMember for unknown actor`` () =
        assertError (ActorNotMember unknownMemberId) (validate unknownMemberId)
