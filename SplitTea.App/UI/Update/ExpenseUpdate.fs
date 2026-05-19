module ExpenseUpdate

open Elmish
open SplitTea.Core
open UITypes
open AppHelpers

let handleAddExpenseClick (model: Model) : Model * Cmd<Msg> =
    let cur     = model.SpaceState.Currency
    let members = sortedMembers model.SpaceState
    { model with Page = AddExpense; ExpenseForm = emptyExpenseForm cur members; EditingExpenseId = None }, Cmd.none

let handleEditExpenseClick (expenseId: ExpenseId) (model: Model) : Model * Cmd<Msg> =
    match Map.tryFind expenseId model.SpaceState.Expenses with
    | None -> model, Cmd.none
    | Some expense ->
        let splitMode, included, customAmounts =
            match expense.Split with
            | Equal memberIds ->
                EqualSplit, Set.ofList memberIds, Map.empty
            | Exact shares ->
                let amounts = shares |> List.map (fun (id, amt) -> id, $"%.2f{amt}") |> Map.ofList
                CustomSplit, Set.empty, amounts
        let prefillRate =
            if expense.PaidCurrency = model.SpaceState.Currency then Field.emptyDecimal
            else
                match Map.tryFind expense.PaidCurrency model.ExchangeRates with
                | Some r -> Field.ofRate r
                | None   -> Field.emptyDecimal
        let form = {
            Description  = expense.Description
            Amount       = Field.ofAmount expense.PaidAmount
            Currency     = expense.PaidCurrency
            ExchangeRate = prefillRate
            PaidById     = expense.PaidBy
            DateText     = $"%04d{expense.Date.Year}-%02d{expense.Date.Month}-%02d{expense.Date.Day}"
            Category     = expense.Category |> Option.defaultValue ""
            Notes        = expense.Notes |> Option.defaultValue ""
            IsSubmitting     = false
            Error            = None
            IsAddingCategory = false
            NewCategoryText  = ""
            SplitMode        = splitMode
            Included         = included
            CustomAmounts    = customAmounts
        }
        { model with Page = AddExpense; ExpenseForm = form; EditingExpenseId = Some expenseId }, Cmd.none

let handleDeleteExpenseClick (expenseId: ExpenseId) (model: Model) : Model * Cmd<Msg> =
    { model with ConfirmDialog = Some { Message = "Delete this expense?"; Action = ConfirmDeleteExpense expenseId } }, Cmd.none

let handleExpenseFormSet (form: ExpenseForm) (model: Model) : Model * Cmd<Msg> =
    { model with ExpenseForm = form }, Cmd.none

let handleExpenseSubmit (model: Model) : Model * Cmd<Msg> =
    match model.ActiveSpaceId with
    | None -> model, Cmd.none
    | Some sid ->
        let form     = model.ExpenseForm
        let paidById = form.PaidById
        match form.Amount.Parsed with
        | None ->
            { model with ExpenseForm = { form with Error = Some "Invalid amount." } }, Cmd.none
        | Some amount ->
            if amount <= 0m then
                { model with ExpenseForm = { form with Error = Some "Amount must be positive." } }, Cmd.none
            elif form.Description.Trim() = "" then
                { model with ExpenseForm = { form with Error = Some "Description is required." } }, Cmd.none
            else
                let actorId = resolveActor model
                let split =
                    match form.SplitMode with
                    | EqualSplit ->
                        form.Included |> Set.toList |> Equal
                    | CustomSplit ->
                        form.CustomAmounts
                        |> Map.toList
                        |> List.choose (fun (id, txt) ->
                            try
                                let amt = decimal txt
                                if amt > 0m then Some (id, amt) else None
                            with _ -> None)
                        |> Exact
                let date     = parseFormDate form.DateText
                let category = if form.Category = "" then None else Some form.Category
                let notes    = if form.Notes.Trim() = "" then None else Some (form.Notes.Trim())
                let cmd =
                    match model.EditingExpenseId, model.EditingExpenseId |> Option.bind (fun eid -> Map.tryFind eid model.SpaceState.Expenses) with
                    | Some _, Some original ->
                        Cmd.OfAsync.either
                            (fun () -> Commands.correctExpense sid actorId original form.Description amount form.Currency paidById split date category notes)
                            ()
                            (fun () -> ExpenseCorrected (Ok ()))
                            (fun ex  -> ExpenseCorrected (Error ex.Message))
                    | Some _, None ->
                        Cmd.ofMsg (ExpenseCorrected (Error "Expense no longer exists."))
                    | None, _ ->
                        Cmd.OfAsync.either
                            (fun () -> Commands.addExpense sid actorId form.Description amount form.Currency paidById split date category notes)
                            ()
                            (fun () -> ExpenseSaved (Ok ()))
                            (fun ex  -> ExpenseSaved (Error ex.Message))
                { model with ExpenseForm = { form with IsSubmitting = true; Error = None } }, cmd

let handleExpenseSaved (result: Result<unit, string>) (model: Model) : Model * Cmd<Msg> =
    match result with
    | Ok () ->
        match model.ActiveSpaceId with
        | Some sid ->
            let loadCmd  = Cmd.OfAsync.perform Storage.rebaseAndDisplay sid SpaceRebased
            let clearCmd = Cmd.OfAsync.perform (fun () -> Async.Sleep Styles.toastDurationMs) () (fun () -> ToastCleared)
            { model with ExpenseForm = { model.ExpenseForm with IsSubmitting = false }; Toast = Some "Expense saved!" },
            Cmd.batch [ loadCmd; clearCmd ]
        | None ->
            { model with Page = SpaceOverview }, Cmd.none
    | Error err ->
        { model with ExpenseForm = { model.ExpenseForm with IsSubmitting = false; Error = Some err } }, Cmd.none

let handleExpenseCorrected (result: Result<unit, string>) (model: Model) : Model * Cmd<Msg> =
    match result with
    | Ok () ->
        match model.ActiveSpaceId with
        | Some sid ->
            let loadCmd  = Cmd.OfAsync.perform Storage.rebaseAndDisplay sid SpaceRebased
            let clearCmd = Cmd.OfAsync.perform (fun () -> Async.Sleep Styles.toastDurationMs) () (fun () -> ToastCleared)
            { model with ExpenseForm = { model.ExpenseForm with IsSubmitting = false }; EditingExpenseId = None; Toast = Some "Expense updated!" },
            Cmd.batch [ loadCmd; clearCmd ]
        | None ->
            { model with Page = SpaceOverview }, Cmd.none
    | Error err ->
        { model with ExpenseForm = { model.ExpenseForm with IsSubmitting = false; Error = Some err } }, Cmd.none

let handleExpenseDeleted (result: Result<unit, string>) (model: Model) : Model * Cmd<Msg> =
    match result with
    | Ok () ->
        match model.ActiveSpaceId with
        | Some sid ->
            let loadCmd  = Cmd.OfAsync.perform Storage.rebaseAndDisplay sid SpaceRebased
            let clearCmd = Cmd.OfAsync.perform (fun () -> Async.Sleep Styles.toastDurationMs) () (fun () -> ToastCleared)
            { model with Toast = Some "Expense deleted!" }, Cmd.batch [ loadCmd; clearCmd ]
        | None -> model, Cmd.none
    | Error err ->
        { model with Toast = Some $"Delete failed: %s{err}" }, Cmd.none
