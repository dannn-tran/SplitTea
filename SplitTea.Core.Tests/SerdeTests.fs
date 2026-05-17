module SplitTea.Core.Tests.SerdeTests

open Xunit
open SplitTea.Core
open SplitTea.Core.Serde
open SplitTea.Core.Tests.Helpers

let private roundtrip event =
    let json = encodeEventJson event
    match decodeEventJson json with
    | Ok decoded -> decoded
    | Error e -> failwith $"Decode failed: {e}\nJSON: {json}"

let private assertRoundtrip event =
    Assert.Equal(event, roundtrip event)

[<Fact>]
let ``SpaceCreated roundtrips`` () =
    assertRoundtrip spaceCreated

[<Fact>]
let ``MemberAdded roundtrips`` () =
    assertRoundtrip aliceAdded

module ``Split roundtrips`` =
    let private baseExpense split =
        ExpenseAdded (envelope aliceId 5 {
            ExpenseId = expense1Id; Description = "Test"
            PaidAmount = 12m; PaidCurrency = "GBP"; ExchangeRate = None; PaidBy = aliceId
            Split = split; Date = date 2024 1 1; Category = None; Notes = None
        })

    [<Fact>]
    let ``Equal`` () = assertRoundtrip (baseExpense (Equal [aliceId; bobId]))

    [<Fact>]
    let ``Exact`` () = assertRoundtrip (baseExpense (Exact [aliceId, 5m; bobId, 7m]))

    [<Fact>]
    let ``Percentage`` () = assertRoundtrip (baseExpense (Percentage [aliceId, 40m; bobId, 60m]))

    [<Fact>]
    let ``Shares`` () = assertRoundtrip (baseExpense (Shares [aliceId, 1; bobId, 2]))

[<Fact>]
let ``ExpenseAdded with all fields roundtrips`` () =
    let event = ExpenseAdded (envelope aliceId 5 {
        ExpenseId = expense1Id; Description = "Dinner"
        PaidAmount = 84m; PaidCurrency = "USD"; ExchangeRate = Some 0.79m; PaidBy = aliceId
        Split = Equal [aliceId; carolId]; Date = date 2024 6 15
        Category = Some "Food"; Notes = Some "Nice restaurant"
    })
    assertRoundtrip event

[<Fact>]
let ``ExpenseAdded with optional fields absent roundtrips`` () =
    assertRoundtrip workedExpense1

module ``ExpenseCorrected Patch roundtrips`` =
    let private correction patch =
        let state = makeBaseState () |> fun s -> Reducer.reduce s workedExpense1
        let event = ExpenseCorrected (envelope aliceId 10 {
            OriginalExpenseId = expense1Id
            Description = None; PaidAmount = None; PaidCurrency = None
            ExchangeRate = patch; PaidBy = None; Split = None; Date = None
            Category = Unchanged; Notes = Unchanged; Reason = None
        })
        event

    [<Fact>]
    let ``Patch Unchanged roundtrips`` () = assertRoundtrip (correction Unchanged)

    [<Fact>]
    let ``Patch Clear roundtrips`` () = assertRoundtrip (correction Clear)

    [<Fact>]
    let ``Patch SetTo roundtrips`` () = assertRoundtrip (correction (SetTo 1.23m))

[<Fact>]
let ``ExpenseCorrected with string Patch roundtrips`` () =
    let event = ExpenseCorrected (envelope aliceId 10 {
        OriginalExpenseId = expense1Id
        Description = Some "Updated"; PaidAmount = Some 90m; PaidCurrency = None
        ExchangeRate = Unchanged; PaidBy = None; Split = None; Date = None
        Category = SetTo "Food"; Notes = Clear; Reason = Some "price correction"
    })
    assertRoundtrip event

[<Fact>]
let ``ExpenseDeleted roundtrips`` () =
    let event = ExpenseDeleted (envelope aliceId 10 { ExpenseId = expense1Id; Reason = Some "duplicate" })
    assertRoundtrip event

[<Fact>]
let ``SettlementRecorded roundtrips`` () =
    let event = SettlementRecorded (envelope carolId 10 {
        SettlementId = settlement1Id; From = carolId; To = aliceId
        Amount = 42m; Currency = "GBP"; ExchangeRate = None; Date = date 2024 1 3; Notes = None
    })
    assertRoundtrip event

[<Fact>]
let ``SettlementRecorded with foreign currency roundtrips`` () =
    let event = SettlementRecorded (envelope carolId 10 {
        SettlementId = settlement1Id; From = carolId; To = aliceId
        Amount = 55m; Currency = "EUR"; ExchangeRate = Some 1.17m
        Date = date 2024 1 3; Notes = Some "via bank transfer"
    })
    assertRoundtrip event

[<Fact>]
let ``CategoryAdded roundtrips`` () =
    let event = CategoryAdded (envelope aliceId 5 { Name = "Food" })
    assertRoundtrip event

[<Fact>]
let ``CategoryRenamed roundtrips`` () =
    let event = CategoryRenamed (envelope aliceId 6 { OldName = "Food"; NewName = "Dining" })
    assertRoundtrip event

[<Fact>]
let ``CategoryArchived roundtrips`` () =
    let event = CategoryArchived (envelope aliceId 7 { Name = "Food" })
    assertRoundtrip event

[<Fact>]
let ``MemberAdded with UserId roundtrips`` () =
    let userId = UserId (System.Guid.Parse "aaaaaaaa-0000-0000-0000-000000000001")
    let event = MemberAdded (envelope aliceId 2 {
        Member = { Id = aliceId; DisplayName = "Alice"; UserId = Some userId }
    })
    assertRoundtrip event

[<Fact>]
let ``decimal precision preserved`` () =
    let event = ExpenseAdded (envelope aliceId 5 {
        ExpenseId = expense1Id; Description = "Test"
        PaidAmount = 99.99m; PaidCurrency = "GBP"; ExchangeRate = None; PaidBy = aliceId
        Split = Equal [aliceId]; Date = date 2024 1 1; Category = None; Notes = None
    })
    let decoded = roundtrip event
    match decoded with
    | ExpenseAdded e -> Assert.Equal(99.99m, e.Payload.PaidAmount)
    | _ -> Assert.Fail("wrong event type")
