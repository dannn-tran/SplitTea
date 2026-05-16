namespace SplitTea.Core

type ExpenseState = {
    ExpenseId: ExpenseId
    Description: string
    Amount: Amount
    Currency: CurrencyCode
    PaidBy: MemberId
    Split: Split
    Date: System.DateOnly
    Notes: string option
    IsDeleted: bool
}

type GroupState = {
    GroupId: GroupId
    Name: string
    Currency: CurrencyCode
    Members: Map<MemberId, Member>
    Expenses: Map<ExpenseId, ExpenseState>
    Settlements: SettlementRecordedPayload list
}

module GroupState =
    let Empty = {
        GroupId = GroupId System.Guid.Empty
        Name = ""
        Currency = ""
        Members = Map.empty
        Expenses = Map.empty
        Settlements = []
    }

module Reducer =
    let reduce (state: GroupState) (event: GroupEvent) : GroupState =
        match event with
        | GroupCreated e ->
            let p = e.Payload
            { GroupState.Empty with
                GroupId = e.GroupId
                Name = p.Name
                Currency = p.Currency }
        | MemberAdded e ->
            let m = e.Payload.Member
            { state with Members = Map.add m.Id m state.Members }
        | ExpenseAdded e ->
            let p = e.Payload
            let expense = {
                ExpenseId = p.ExpenseId
                Description = p.Description
                Amount = p.Amount
                Currency = p.Currency
                PaidBy = p.PaidBy
                Split = p.Split
                Date = p.Date
                Notes = p.Notes
                IsDeleted = false
            }
            { state with Expenses = Map.add p.ExpenseId expense state.Expenses }
        | ExpenseCorrected e ->
            let p = e.Payload
            match Map.tryFind p.OriginalExpenseId state.Expenses with
            | None -> state
            | Some existing ->
                let corrected = {
                    existing with
                        Description = p.Description |> Option.defaultValue existing.Description
                        Amount      = p.Amount      |> Option.defaultValue existing.Amount
                        Currency    = p.Currency    |> Option.defaultValue existing.Currency
                        PaidBy      = p.PaidBy      |> Option.defaultValue existing.PaidBy
                        Split       = p.Split       |> Option.defaultValue existing.Split
                        Date        = p.Date        |> Option.defaultValue existing.Date
                        // string option option: None = unchanged, Some x = set to x (x may be None)
                        Notes       = p.Notes       |> Option.defaultWith (fun () -> existing.Notes)
                }
                { state with Expenses = Map.add p.OriginalExpenseId corrected state.Expenses }
        | ExpenseDeleted e ->
            let p = e.Payload
            match Map.tryFind p.ExpenseId state.Expenses with
            | None -> state
            | Some existing ->
                { state with Expenses = Map.add p.ExpenseId { existing with IsDeleted = true } state.Expenses }
        | SettlementRecorded e ->
            { state with Settlements = state.Settlements @ [ e.Payload ] }

    let replayEvents (events: GroupEvent list) : GroupState =
        List.fold reduce GroupState.Empty events
