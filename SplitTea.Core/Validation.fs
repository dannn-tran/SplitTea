namespace SplitTea.Core

type ValidationError =
    | UnknownMember         of MemberId
    | UnknownExpense        of ExpenseId
    | UnknownCategory       of string
    | ArchivedCategory      of string
    | DeletedExpense        of ExpenseId
    | AmountMustBePositive
    | SplitMustHaveMembers
    | ExactSplitSumMismatch of expected: Amount * actual: Amount
    | PercentageSumMismatch of expected: decimal * actual: decimal
    | SharesMustBePositive
    | SelfSettlement
    | CurrencyMismatch      of expected: CurrencyCode * actual: CurrencyCode

module Validation =
    let private checkAmount (amount: Amount) =
        if amount <= 0m then [ AmountMustBePositive ] else []

    let private checkCurrency (expected: CurrencyCode) (actual: CurrencyCode) =
        if actual <> expected then [ CurrencyMismatch(expected, actual) ] else []

    let private checkMember (members: Map<MemberId, Member>) (id: MemberId) =
        if Map.containsKey id members then [] else [ UnknownMember id ]

    let private checkCategory (categories: Map<string, CategoryState>) (category: string option) =
        match category with
        | None -> []
        | Some c ->
            match Map.tryFind c categories with
            | None -> [ UnknownCategory c ]
            | Some cat when cat.IsArchived -> [ ArchivedCategory c ]
            | Some _ -> []

    let private checkSplit (split: Split) (members: Map<MemberId, Member>) (amount: Amount) =
        match split with
        | Equal ms ->
            if List.isEmpty ms then [ SplitMustHaveMembers ]
            else ms |> List.collect (checkMember members)
        | Exact shares ->
            if List.isEmpty shares then [ SplitMustHaveMembers ]
            else
                let memberErrs = shares |> List.collect (fun (m, _) -> checkMember members m)
                let total = shares |> List.sumBy snd
                let sumErr = if total <> amount then [ ExactSplitSumMismatch(amount, total) ] else []
                memberErrs @ sumErr
        | Percentage shares ->
            if List.isEmpty shares then [ SplitMustHaveMembers ]
            else
                let memberErrs = shares |> List.collect (fun (m, _) -> checkMember members m)
                let total = shares |> List.sumBy snd
                let sumErr = if total <> 100m then [ PercentageSumMismatch(100m, total) ] else []
                memberErrs @ sumErr
        | Shares shares ->
            if List.isEmpty shares then [ SplitMustHaveMembers ]
            else
                let memberErrs = shares |> List.collect (fun (m, _) -> checkMember members m)
                let shareErrs =
                    shares
                    |> List.choose (fun (_, s) -> if s <= 0 then Some SharesMustBePositive else None)
                    |> List.distinct
                memberErrs @ shareErrs

    let validateEvent (state: SpaceState) (event: SpaceEvent) : Result<SpaceEvent, ValidationError list> =
        let errors =
            match event with
            | SpaceCreated _ -> []
            | MemberAdded _ -> []
            | ExpenseAdded e ->
                let p = e.Payload
                checkAmount p.PaidAmount
                @ checkMember state.Members p.PaidBy
                @ checkSplit p.Split state.Members p.PaidAmount
                @ checkCategory state.Categories p.Category
            | ExpenseCorrected e ->
                let p = e.Payload
                match Map.tryFind p.OriginalExpenseId state.Expenses with
                | None -> [ UnknownExpense p.OriginalExpenseId ]
                | Some ex when ex.IsDeleted -> [ DeletedExpense p.OriginalExpenseId ]
                | Some ex ->
                    let effectiveAmount = p.PaidAmount |> Option.defaultValue ex.PaidAmount
                    let effectiveSplit  = p.Split      |> Option.defaultValue ex.Split
                    let amountErrs  = p.PaidAmount |> Option.map checkAmount                 |> Option.defaultValue []
                    let paidByErrs  = p.PaidBy     |> Option.map (checkMember state.Members) |> Option.defaultValue []
                    let effectiveCategory =
                        match p.Category with
                        | Unchanged -> ex.Category
                        | Clear     -> None
                        | SetTo v   -> Some v
                    let categoryErrs = checkCategory state.Categories effectiveCategory
                    let splitErrs =
                        match p.Split, p.PaidAmount with
                        | None, None -> []
                        | _          -> checkSplit effectiveSplit state.Members effectiveAmount
                    amountErrs @ paidByErrs @ splitErrs @ categoryErrs
            | ExpenseDeleted e ->
                let p = e.Payload
                match Map.tryFind p.ExpenseId state.Expenses with
                | None -> [ UnknownExpense p.ExpenseId ]
                | Some ex when ex.IsDeleted -> [ DeletedExpense p.ExpenseId ]
                | Some _ -> []
            | SettlementRecorded e ->
                let p = e.Payload
                checkMember state.Members p.From
                @ checkMember state.Members p.To
                @ (if p.From = p.To then [ SelfSettlement ] else [])
                @ checkAmount p.Amount
            | CategoryAdded _ -> []
            | CategoryRenamed _ -> []
            | CategoryArchived _ -> []
            | SpaceRenamed _ -> []
            | MemberRenamed _ -> []

        if List.isEmpty errors then Ok event else Error errors
