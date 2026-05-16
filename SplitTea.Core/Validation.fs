namespace SplitTea.Core

type ValidationError =
    | UnknownMember         of MemberId
    | UnknownExpense        of ExpenseId
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

    let private checkSplit (split: Split) (members: Map<MemberId, Member>) (amount: Amount) =
        match split with
        | Equal ms ->
            if List.isEmpty ms then [ SplitMustHaveMembers ]
            else ms |> List.collect (checkMember members)
        | Exact shares ->
            if Map.isEmpty shares then [ SplitMustHaveMembers ]
            else
                let memberErrs = shares |> Map.toList |> List.collect (fun (m, _) -> checkMember members m)
                let total = shares |> Map.toSeq |> Seq.sumBy snd
                let sumErr = if total <> amount then [ ExactSplitSumMismatch(amount, total) ] else []
                memberErrs @ sumErr
        | Percentage shares ->
            if Map.isEmpty shares then [ SplitMustHaveMembers ]
            else
                let memberErrs = shares |> Map.toList |> List.collect (fun (m, _) -> checkMember members m)
                let total = shares |> Map.toSeq |> Seq.sumBy snd
                let sumErr = if total <> 100m then [ PercentageSumMismatch(100m, total) ] else []
                memberErrs @ sumErr
        | Shares shares ->
            if Map.isEmpty shares then [ SplitMustHaveMembers ]
            else
                let memberErrs = shares |> Map.toList |> List.collect (fun (m, _) -> checkMember members m)
                let shareErrs =
                    shares
                    |> Map.toList
                    |> List.choose (fun (_, s) -> if s <= 0 then Some SharesMustBePositive else None)
                    |> List.distinct
                memberErrs @ shareErrs

    let validateEvent (state: GroupState) (event: GroupEvent) : Result<GroupEvent, ValidationError list> =
        let errors =
            match event with
            | GroupCreated _ -> []
            | MemberAdded _ -> []
            | ExpenseAdded e ->
                let p = e.Payload
                checkAmount p.Amount
                @ checkCurrency state.Currency p.Currency
                @ checkMember state.Members p.PaidBy
                @ checkSplit p.Split state.Members p.Amount
            | ExpenseCorrected e ->
                let p = e.Payload
                match Map.tryFind p.OriginalExpenseId state.Expenses with
                | None -> [ UnknownExpense p.OriginalExpenseId ]
                | Some ex when ex.IsDeleted -> [ DeletedExpense p.OriginalExpenseId ]
                | Some ex ->
                    let effectiveAmount = p.Amount |> Option.defaultValue ex.Amount
                    let effectiveSplit  = p.Split  |> Option.defaultValue ex.Split
                    let amountErrs   = p.Amount   |> Option.map checkAmount                    |> Option.defaultValue []
                    let currencyErrs = p.Currency |> Option.map (checkCurrency state.Currency) |> Option.defaultValue []
                    let paidByErrs   = p.PaidBy   |> Option.map (checkMember state.Members)    |> Option.defaultValue []
                    let splitErrs =
                        match p.Split, p.Amount with
                        | None, None -> []
                        | _          -> checkSplit effectiveSplit state.Members effectiveAmount
                    amountErrs @ currencyErrs @ paidByErrs @ splitErrs
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
                @ checkCurrency state.Currency p.Currency

        if List.isEmpty errors then Ok event else Error errors
