namespace SplitTea.Core

type ExpenseState = {
    ExpenseId    : ExpenseId
    Description  : string
    PaidAmount   : Amount
    PaidCurrency : CurrencyCode
    ExchangeRate : decimal option
    PaidBy       : MemberId
    Split        : Split
    Date         : System.DateOnly
    Category     : string option
    Notes        : string option
    IsDeleted    : bool
}

type SpaceState = {
    SpaceId     : SpaceId
    Name        : string
    Currency    : CurrencyCode
    Members     : Map<MemberId, Member>
    Categories  : Map<string, CategoryState>
    Expenses    : Map<ExpenseId, ExpenseState>
    Settlements : SettlementRecordedPayload list
}

module SpaceState =
    let Empty = {
        SpaceId     = SpaceId System.Guid.Empty
        Name        = ""
        Currency    = ""
        Members     = Map.empty
        Categories  = Map.empty
        Expenses    = Map.empty
        Settlements = []
    }

module Reducer =
    let reduce (state: SpaceState) (event: SpaceEvent) : SpaceState =
        match event with
        | SpaceCreated e ->
            let p = e.Payload
            let categories =
                p.Categories
                |> List.map (fun name -> name, { Name = name; IsArchived = false })
                |> Map.ofList
            { SpaceState.Empty with
                SpaceId    = e.SpaceId
                Name       = p.Name
                Currency   = p.Currency
                Categories = categories }
        | MemberAdded e ->
            let m = e.Payload.Member
            { state with Members = Map.add m.Id m state.Members }
        | CategoryAdded e ->
            let name = e.Payload.Name.Trim()
            if name = "" then state
            else
                { state with Categories = Map.add name { Name = name; IsArchived = false } state.Categories }
        | CategoryRenamed e ->
            let oldName = e.Payload.OldName.Trim()
            let newName = e.Payload.NewName.Trim()
            if oldName = "" || newName = "" || oldName = newName then state
            else
                let categories =
                    state.Categories
                    |> Map.remove oldName
                    |> Map.add newName { Name = newName; IsArchived = false }
                let expenses =
                    state.Expenses
                    |> Map.map (fun _ ex ->
                        if ex.Category = Some oldName then { ex with Category = Some newName } else ex)
                { state with Categories = categories; Expenses = expenses }
        | CategoryArchived e ->
            let name = e.Payload.Name.Trim()
            match Map.tryFind name state.Categories with
            | None -> state
            | Some cat ->
                { state with Categories = Map.add name { cat with IsArchived = true } state.Categories }
        | ExpenseAdded e ->
            let p = e.Payload
            let expense = {
                ExpenseId    = p.ExpenseId
                Description  = p.Description
                PaidAmount   = p.PaidAmount
                PaidCurrency = p.PaidCurrency
                ExchangeRate = p.ExchangeRate
                PaidBy       = p.PaidBy
                Split        = p.Split
                Date         = p.Date
                Category     = p.Category
                Notes        = p.Notes
                IsDeleted    = false
            }
            { state with Expenses = Map.add p.ExpenseId expense state.Expenses }
        | ExpenseCorrected e ->
            let p = e.Payload
            match Map.tryFind p.OriginalExpenseId state.Expenses with
            | None -> state
            | Some existing ->
                let applyPatch current = function
                    | Unchanged -> current
                    | Clear     -> None
                    | SetTo v   -> Some v
                let corrected = {
                    existing with
                        Description  = p.Description  |> Option.defaultValue existing.Description
                        PaidAmount   = p.PaidAmount   |> Option.defaultValue existing.PaidAmount
                        PaidCurrency = p.PaidCurrency |> Option.defaultValue existing.PaidCurrency
                        ExchangeRate = applyPatch existing.ExchangeRate p.ExchangeRate
                        PaidBy       = p.PaidBy       |> Option.defaultValue existing.PaidBy
                        Split        = p.Split        |> Option.defaultValue existing.Split
                        Date         = p.Date         |> Option.defaultValue existing.Date
                        Category     = applyPatch existing.Category p.Category
                        Notes        = applyPatch existing.Notes    p.Notes
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

    let replayEvents (events: SpaceEvent list) : SpaceState =
        List.fold reduce SpaceState.Empty events
