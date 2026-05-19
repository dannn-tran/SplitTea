namespace SplitTea.Core

type ValidationError =
    | UnknownMember           of MemberId
    | UnknownExpense          of ExpenseId
    | UnknownCategory         of string
    | ArchivedCategory        of string
    | DeletedExpense          of ExpenseId
    | AmountMustBePositive
    | SplitMustHaveMembers
    | ExactSplitSumMismatch   of expected: Amount * actual: Amount
    | PercentageSumMismatch   of expected: decimal * actual: decimal
    | SharesMustBePositive
    | SelfSettlement
    | CurrencyMismatch        of expected: CurrencyCode * actual: CurrencyCode
    | ActorNotMember          of MemberId
    | CannotRenameOtherMember
    | DuplicateMember         of MemberId

module Validation =
    let private checkActor (members: Map<MemberId, Member>) (actorId: MemberId) =
        if Map.containsKey actorId members then [] else [ ActorNotMember actorId ]

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

    let validateEvent (state: SpaceState) (event: SpaceEvent) : Result<SpaceEvent, ValidationError list> =
        let errors =
            match event with
            | SpaceCreated _ -> []
            | MemberAdded e ->
                let id = e.Payload.Member.Id
                if Map.containsKey id state.Members then [ DuplicateMember id ] else []
            | ExpenseAdded e ->
                let p = e.Payload
                checkActor state.Members e.ActorId
                @ checkAmount p.PaidAmount
                @ checkMember state.Members p.PaidBy
                @ checkSplit p.Split state.Members p.PaidAmount
                @ checkCategory state.Categories p.Category
            | ExpenseCorrected e ->
                let p = e.Payload
                checkActor state.Members e.ActorId
                @ match Map.tryFind p.OriginalExpenseId state.Expenses with
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
                checkActor state.Members e.ActorId
                @ match Map.tryFind p.ExpenseId state.Expenses with
                  | None -> [ UnknownExpense p.ExpenseId ]
                  | Some ex when ex.IsDeleted -> [ DeletedExpense p.ExpenseId ]
                  | Some _ -> []
            | SettlementRecorded e ->
                let p = e.Payload
                checkActor state.Members e.ActorId
                @ checkMember state.Members p.From
                @ checkMember state.Members p.To
                @ (if p.From = p.To then [ SelfSettlement ] else [])
                @ (if List.isEmpty p.Payments then [ AmountMustBePositive ]
                   else p.Payments |> List.collect (fun leg -> checkAmount leg.Amount))
            | SpaceRenamed e    -> checkActor state.Members e.ActorId
            | CategoryAdded e   -> checkActor state.Members e.ActorId
            | CategoryRenamed e -> checkActor state.Members e.ActorId
            | CategoryArchived e -> checkActor state.Members e.ActorId
            | MemberRenamed e ->
                checkActor state.Members e.ActorId
                @ checkMember state.Members e.Payload.MemberId
                @ (if e.ActorId <> e.Payload.MemberId then [ CannotRenameOtherMember ] else [])

        if List.isEmpty errors then Ok event else Error errors
