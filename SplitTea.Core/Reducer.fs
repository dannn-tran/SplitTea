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
    ContextId    : ContextId option
    IsDeleted    : bool
}

type ContextState = {
    ContextId : ContextId
    Name      : string
    Template  : ContextTemplate
    Members   : MemberId list option
    DateFrom  : System.DateOnly option
    DateTo    : System.DateOnly option
}

type GroupState = {
    GroupId     : GroupId
    Name        : string
    Currency    : CurrencyCode
    Members     : Map<MemberId, Member>
    Contexts    : Map<ContextId, ContextState>
    Expenses    : Map<ExpenseId, ExpenseState>
    Settlements : SettlementRecordedPayload list
}

module GroupState =
    let Empty = {
        GroupId     = GroupId System.Guid.Empty
        Name        = ""
        Currency    = ""
        Members     = Map.empty
        Contexts    = Map.empty
        Expenses    = Map.empty
        Settlements = []
    }

module Reducer =
    let reduce (state: GroupState) (event: GroupEvent) : GroupState =
        match event with
        | GroupCreated e ->
            let p = e.Payload
            { GroupState.Empty with
                GroupId  = e.GroupId
                Name     = p.Name
                Currency = p.Currency }
        | MemberAdded e ->
            let m = e.Payload.Member
            { state with Members = Map.add m.Id m state.Members }
        | ContextCreated e ->
            let p = e.Payload
            let ctx = {
                ContextId = p.ContextId
                Name      = p.Name
                Template  = p.Template
                Members   = p.Members
                DateFrom  = p.DateFrom
                DateTo    = p.DateTo
            }
            { state with Contexts = Map.add p.ContextId ctx state.Contexts }
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
                ContextId    = p.ContextId
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
                        ContextId    = applyPatch existing.ContextId p.ContextId
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
