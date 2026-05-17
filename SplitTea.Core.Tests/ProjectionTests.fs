module SplitTea.Core.Tests.ProjectionTests

open Xunit
open SplitTea.Core
open SplitTea.Core.Projections
open SplitTea.Core.Tests.Helpers

module ``computeNetPositions`` =
    [<Fact>]
    let ``worked example: Alice +42 Bob +82 Carol -124`` () =
        let positions = computeNetPositions (makeWorkedState ())
        Assert.Equal(42m,   (findPosition aliceId positions).Amount)
        Assert.Equal(82m,   (findPosition bobId   positions).Amount)
        Assert.Equal(-124m, (findPosition carolId positions).Amount)

    [<Fact>]
    let ``Equal split payer absorbs rounding remainder`` () =
        // £10 / 3 = 3.34 each; remainder = -0.02 → payer gets 3.32
        // non-payers each owe 3.34; payer.net = 6.68
        let expense = ExpenseAdded (envelope aliceId 5 {
            ExpenseId = expense3Id; Description = "Rounding test"
            PaidAmount = 10m; PaidCurrency = "GBP"; ExchangeRate = None; PaidBy = aliceId
            Split = Equal [aliceId; bobId; carolId]
            Date = date 2024 1 1; Category = None; Notes = None
        })
        let state = Reducer.replayEvents [
            spaceCreated; aliceAdded; bobAdded; carolAdded; expense
        ]
        let positions = computeNetPositions state
        Assert.Equal(6.68m,  (findPosition aliceId positions).Amount)
        Assert.Equal(-3.34m, (findPosition bobId   positions).Amount)
        Assert.Equal(-3.34m, (findPosition carolId positions).Amount)

    [<Fact>]
    let ``deleted expense excluded from positions`` () =
        let state0 = makeBaseState () |> fun s -> Reducer.reduce s workedExpense1
        let deletion = ExpenseDeleted (envelope aliceId 10 { ExpenseId = expense1Id; Reason = None })
        let state1 = Reducer.reduce state0 deletion
        let positions = computeNetPositions state1
        Assert.Equal(0m, (findPosition aliceId positions).Amount)
        Assert.Equal(0m, (findPosition carolId positions).Amount)

    [<Fact>]
    let ``settlement reduces balance`` () =
        // Carol pays Alice 42 → Alice 0, Bob +82, Carol -82
        let settle = SettlementRecorded (envelope carolId 10 {
            SettlementId = settlement1Id; From = carolId; To = aliceId
            Amount = 42m; Currency = "GBP"; ExchangeRate = None; Date = date 2024 1 3; Notes = None
        })
        let state = Reducer.replayEvents [
            spaceCreated; aliceAdded; bobAdded; carolAdded
            workedExpense1; workedExpense2; settle
        ]
        let positions = computeNetPositions state
        Assert.Equal(0m,   (findPosition aliceId positions).Amount)
        Assert.Equal(82m,  (findPosition bobId   positions).Amount)
        Assert.Equal(-82m, (findPosition carolId positions).Amount)

    [<Fact>]
    let ``state with no expenses returns all-zero positions`` () =
        let positions = computeNetPositions (makeBaseState ())
        Assert.All(positions, fun p -> Assert.Equal(0m, p.Amount))

module ``computeMinimumSettlements`` =
    [<Fact>]
    let ``already balanced returns empty list`` () =
        let positions = [
            { MemberId = aliceId; Amount =  10m }
            { MemberId = bobId;   Amount = -10m }
        ]
        let s = computeMinimumSettlements [{ MemberId = aliceId; Amount = 0m }]
        Assert.Empty(s)

    [<Fact>]
    let ``one creditor one debtor equal amounts`` () =
        let positions = [
            { MemberId = aliceId; Amount =  42m }
            { MemberId = carolId; Amount = -42m }
        ]
        let settlements = computeMinimumSettlements positions
        Assert.Equal(1, List.length settlements)
        Assert.Equal(carolId, settlements.[0].From)
        Assert.Equal(aliceId, settlements.[0].To)
        Assert.Equal(42m, settlements.[0].Amount)

    [<Fact>]
    let ``worked example produces minimum two settlements`` () =
        let positions = computeNetPositions (makeWorkedState ())
        let settlements = computeMinimumSettlements positions
        Assert.Equal(2, List.length settlements)
        Assert.True(List.contains { From = carolId; To = bobId;   Amount = 82m } settlements)
        Assert.True(List.contains { From = carolId; To = aliceId; Amount = 42m } settlements)

    [<Fact>]
    let ``one creditor two debtors splits across two settlements`` () =
        // Bob +30, Alice -10, Carol -20 → Alice pays Bob 10, Carol pays Bob 20
        let positions = [
            { MemberId = bobId;   Amount =  30m }
            { MemberId = aliceId; Amount = -10m }
            { MemberId = carolId; Amount = -20m }
        ]
        let settlements = computeMinimumSettlements positions
        Assert.Equal(2, List.length settlements)
        Assert.True(List.contains { From = carolId; To = bobId; Amount = 20m } settlements)
        Assert.True(List.contains { From = aliceId; To = bobId; Amount = 10m } settlements)
