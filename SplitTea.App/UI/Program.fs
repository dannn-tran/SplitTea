module AppRoot

open Elmish
open Feliz
open Fable.Core
open Fable.Core.JsInterop
open SplitTea.Core
open UITypes

let private browserStorage : obj = emitJsExpr () "localStorage"
#if DEVMODE
let private sessionStore   : obj = emitJsExpr () "sessionStorage"
let private getDevActorId () : MemberId option =
    let v : obj = sessionStore?getItem("devActorId")
    if isNull v then None
    else try Some (MemberId (System.Guid.Parse (string v))) with _ -> None
let private setDevActorId (id: MemberId) =
    let (MemberId g) = id
    sessionStore?setItem("devActorId", string g) |> ignore
#endif

let private getActiveSpaceId () : SpaceId option =
    let v : obj = browserStorage?getItem("activeSpaceId")
    if isNull v then None
    else
        try Some (SpaceId (System.Guid.Parse (string v)))
        with _ -> None

let private setActiveSpaceId (id: SpaceId) =
    let (SpaceId g) = id
    browserStorage?setItem("activeSpaceId", string g) |> ignore

let private clearActiveSpaceId () =
    browserStorage?removeItem("activeSpaceId") |> ignore

let private todayStr () =
    let d = System.DateTime.Now
    sprintf "%04d-%02d-%02d" d.Year d.Month d.Day

let private parseFormDate (s: string) =
    if s = "" then
        let d = System.DateTime.Now
        System.DateOnly(d.Year, d.Month, d.Day)
    else
        let parts = s.Split('-')
        System.DateOnly(int parts.[0], int parts.[1], int parts.[2])

let private sortedMembers (state: SpaceState) =
    state.Members
    |> Map.toList
    |> List.map snd
    |> List.sortBy (fun m -> m.DisplayName)

let private findActorId (state: SpaceState) (user: Auth.AuthUser option) : MemberId =
    let first () = state.Members |> Map.toList |> List.head |> fst
    match user with
    | None -> first ()
    | Some u ->
        let uid = UserId (System.Guid.Parse u.Id)
        state.Members
        |> Map.toSeq
        |> Seq.tryFind (fun (_, m) -> m.UserId = Some uid)
        |> Option.map fst
        |> Option.defaultWith first

let private resolveActor (model: Model) : MemberId =
    let first () = model.SpaceState.Members |> Map.toList |> List.head |> fst
#if DEVMODE
    if DevMode.isEnabled () then
        match model.DevActorId with
        | Some id when Map.containsKey id model.SpaceState.Members -> id
        | _ -> first ()
    else
#endif
    findActorId model.SpaceState model.Auth

let private emptyCreateSpaceForm : CreateSpaceForm = {
    SpaceNameText = ""
    CurrencyText  = ""
    MemberName    = ""
    IsSubmitting  = false
    Error         = None
}

let private emptyExpenseForm (groupCurrency: string) (memberCount: int) : ExpenseForm = {
    Description      = ""
    AmountText       = ""
    Currency         = groupCurrency
    ExchangeRateText = ""
    PaidByIndex      = 0
    DateText         = todayStr ()
    Category         = ""
    Notes            = ""
    IsSubmitting     = false
    Error            = None
    IsAddingCategory = false
    NewCategoryText  = ""
    SplitMode        = EqualSplit
    IncludedIndices  = Set.ofList [ 0 .. memberCount - 1 ]
    CustomAmounts    = Map.empty
}

let private emptySettlementForm (memberCount: int) (groupCurrency: string) : SettlementForm = {
    FromIndex         = 0
    ToIndex           = if memberCount > 1 then 1 else 0
    AmountText        = ""
    Currency          = groupCurrency
    ExchangeRateText  = ""
    UseSecondPayment  = false
    AmountText2       = ""
    Currency2         = groupCurrency
    ExchangeRateText2 = ""
    DateText          = todayStr ()
    Notes             = ""
    IsSubmitting      = false
    Error             = None
}

let init () : Model * Cmd<Msg> =
    let activeSpaceId = getActiveSpaceId ()
    let model = {
        Auth           = None
        Page           = Loading
        ActiveSpaceId  = activeSpaceId
        SpaceState     = SpaceState.Empty
        CategoryFilter = ""
        NewCategory    = ""
        EditingCategory = None
        EditCategoryName = ""
        CategoryError    = None
        EditingExpenseId = None
        ExchangeRates    = Map.empty
        SignInEmail    = ""
        SignInError    = None
        IsAuthLoading  = true
        ExpenseForm    = emptyExpenseForm "" 0
        SettlementForm = emptySettlementForm 0 ""
        Toast                = None
        ShowSettings         = false
        IsEditingSpaceName   = false
        SpaceNameText        = ""
        IsEditingProfileName = false
        ProfileNameText      = ""
        ProfileNameError     = None
        ShowSpaceSwitcher    = false
        Conflicts            = []
        KnownSpaces          = Storage.getKnownSpaces ()
        CreateSpaceForm      = emptyCreateSpaceForm
#if DEVMODE
        DevActorId     = getDevActorId ()
#endif
    }
    let cmd =
        match activeSpaceId with
        | Some sid ->
            let seedCmd =
                if not (Storage.getKnownSpaces () |> List.exists (fun s -> s.Id = sid)) then
                    Cmd.OfFunc.attempt (fun () -> Storage.upsertKnownSpace sid "") () (fun _ -> SyncDone)
                else Cmd.none
            let loadCmd = Cmd.OfAsync.perform Storage.loadSpaceState sid (fun ss -> SpaceLoaded (sid, ss))
            Cmd.batch [ seedCmd; loadCmd ]
        | None -> Cmd.none
    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | AuthReceived (Auth.SignedIn user) ->
        let page =
            match model.Page with
            | Loading ->
                if model.ActiveSpaceId.IsSome then Loading
#if DEVMODE
                elif DevMode.isEnabled () then DevBootstrap
#endif
                else CreateSpace
            | p -> p
        { model with Auth = Some user; IsAuthLoading = false; Page = page }, Cmd.none

    | AuthReceived Auth.SignedOut ->
        let page =
#if DEVMODE
            if DevMode.isEnabled () then DevBootstrap else SignIn
#else
            SignIn
#endif
        { model with Auth = None; IsAuthLoading = false; Page = page }, Cmd.none

#if DEVMODE
    | CreateDevSpace ->
        let cmd =
            Cmd.OfAsync.either
                DevBootstrap.createLocalSpace
                ()
                (fun sid -> DevSpaceCreated (Ok sid))
                (fun ex -> DevSpaceCreated (Error ex.Message))
        { model with IsAuthLoading = true }, cmd

    | DevSpaceCreated (Ok sid) ->
        let cmd = Cmd.OfAsync.perform Storage.loadSpaceState sid (fun ss -> SpaceLoaded (sid, ss))
        { model with IsAuthLoading = false }, cmd

    | DevSpaceCreated (Error _) ->
        { model with IsAuthLoading = false }, Cmd.none
#endif

    | SignInEmailSet email ->
        { model with SignInEmail = email; SignInError = None }, Cmd.none

    | CategoryFilterSet category ->
        { model with CategoryFilter = category }, Cmd.none

    | NewCategorySet name ->
        { model with NewCategory = name; CategoryError = None }, Cmd.none

    | StartCategoryRename name ->
        { model with EditingCategory = Some name; EditCategoryName = name; CategoryError = None }, Cmd.none

    | EditCategoryNameSet name ->
        { model with EditCategoryName = name; CategoryError = None }, Cmd.none

    | SignInSubmit ->
        let cmd =
            Cmd.OfAsync.perform
                (fun () -> Auth.signInWithMagicLink model.SignInEmail)
                ()
                SignInDone
        { model with IsAuthLoading = true }, cmd

    | SignInDone (Ok ()) ->
        { model with IsAuthLoading = false; SignInError = Some "Check your email for the magic link." }, Cmd.none

    | SignInDone (Error err) ->
        { model with IsAuthLoading = false; SignInError = Some err }, Cmd.none

    | SignOut ->
        let cmd = Cmd.OfAsync.attempt (fun () -> Auth.signOut ()) () (fun _ -> AuthReceived Auth.SignedOut)
        model, cmd

    | SpaceLoaded (sid, ss) ->
        setActiveSpaceId sid
        Storage.upsertKnownSpace sid ss.Name
        let authToken = model.Auth |> Option.map (fun u -> u.AccessToken) |> Option.defaultValue ""
        let syncCmd   = Cmd.OfAsync.perform (fun () -> Sync.pushPending authToken) () (fun outcomes -> SyncOutcome (sid, outcomes))
        let rebaseCmd = Cmd.OfAsync.perform Storage.rebaseAndDisplay sid SpaceRebased
        let fxCmd     =
            Cmd.OfAsync.either
                (fun () -> FxRates.getRates ss.Currency)
                ()
                ExchangeRatesLoaded
                (fun _ -> ExchangeRatesLoaded Map.empty)
        { model with ActiveSpaceId = Some sid; SpaceState = ss; Page = SpaceOverview
                     KnownSpaces = Storage.getKnownSpaces () }, Cmd.batch [ syncCmd; rebaseCmd; fxCmd ]

    | ExchangeRatesLoaded rates ->
        { model with ExchangeRates = rates }, Cmd.none

    | SpaceNotFound ->
        { model with Page = SignIn }, Cmd.none

    | AddCategorySubmit ->
        match model.ActiveSpaceId with
        | None -> model, Cmd.none
        | Some sid ->
            let name = model.NewCategory.Trim()
            if name = "" then
                { model with CategoryError = Some "Category name is required." }, Cmd.none
            elif model.SpaceState.Categories |> Map.containsKey name then
                { model with CategoryError = Some "Category already exists." }, Cmd.none
            else
                let actorId = resolveActor model
                let cmd =
                    Cmd.OfAsync.either
                        (fun () -> Commands.addCategory sid actorId name)
                        ()
                        (fun () -> CategorySaved (Ok ()))
                        (fun ex -> CategorySaved (Error ex.Message))
                { model with IsAuthLoading = true; CategoryError = None }, cmd

    | SaveCategoryRename ->
        match model.ActiveSpaceId, model.EditingCategory with
        | Some sid, Some oldName ->
            let newName = model.EditCategoryName.Trim()
            if newName = "" then
                { model with CategoryError = Some "Category name is required." }, Cmd.none
            elif oldName <> newName && model.SpaceState.Categories |> Map.containsKey newName then
                { model with CategoryError = Some "Category already exists." }, Cmd.none
            else
                let actorId = resolveActor model
                let cmd =
                    Cmd.OfAsync.either
                        (fun () -> Commands.renameCategory sid actorId oldName newName)
                        ()
                        (fun () -> CategorySaved (Ok ()))
                        (fun ex -> CategorySaved (Error ex.Message))
                { model with IsAuthLoading = true; CategoryError = None }, cmd
        | _ -> model, Cmd.none

    | ArchiveCategory name ->
        match model.ActiveSpaceId with
        | None -> model, Cmd.none
        | Some sid ->
            let actorId = resolveActor model
            let cmd =
                Cmd.OfAsync.either
                    (fun () -> Commands.archiveCategory sid actorId name)
                    ()
                    (fun () -> CategorySaved (Ok ()))
                    (fun ex -> CategorySaved (Error ex.Message))
            { model with IsAuthLoading = true; CategoryError = None }, cmd

    | CategorySaved (Ok ()) ->
        match model.ActiveSpaceId with
        | Some sid ->
            let cmd = Cmd.OfAsync.perform Storage.loadSpaceState sid (fun ss -> SpaceLoaded (sid, ss))
            { model with
                IsAuthLoading = false
                NewCategory = ""
                EditingCategory = None
                EditCategoryName = ""
                CategoryError = None }, cmd
        | None ->
            { model with IsAuthLoading = false }, Cmd.none

    | CategorySaved (Error err) ->
        { model with IsAuthLoading = false; CategoryError = Some err }, Cmd.none

    | NavigateTo page ->
        { model with Page = page }, Cmd.none

    | AddExpenseClick ->
        let cur = model.SpaceState.Currency
        let n   = model.SpaceState.Members.Count
        { model with Page = AddExpense; ExpenseForm = emptyExpenseForm cur n; EditingExpenseId = None }, Cmd.none

    | EditExpenseClick expenseId ->
        match Map.tryFind expenseId model.SpaceState.Expenses with
        | None -> model, Cmd.none
        | Some expense ->
            let members = sortedMembers model.SpaceState
            let findIdx id = members |> List.tryFindIndex (fun m -> m.Id = id)
            let paidByIndex = findIdx expense.PaidBy |> Option.defaultValue 0
            let splitMode, includedIndices, customAmounts =
                match expense.Split with
                | Equal memberIds ->
                    let indices = memberIds |> List.choose findIdx |> Set.ofList
                    EqualSplit, indices, Map.empty
                | Exact shares ->
                    let amounts =
                        shares
                        |> List.choose (fun (id, amt) -> findIdx id |> Option.map (fun i -> i, sprintf "%.2f" amt))
                        |> Map.ofList
                    CustomSplit, Set.empty, amounts
                | Percentage shares ->
                    let amounts =
                        shares
                        |> List.choose (fun (id, pct) ->
                            findIdx id |> Option.map (fun i -> i, sprintf "%.2f" (expense.PaidAmount * pct / 100m)))
                        |> Map.ofList
                    CustomSplit, Set.empty, amounts
                | Shares shares ->
                    let total = shares |> List.sumBy snd
                    let amounts =
                        shares
                        |> List.choose (fun (id, s) ->
                            findIdx id |> Option.map (fun i -> i, sprintf "%.2f" (expense.PaidAmount * decimal s / decimal total)))
                        |> Map.ofList
                    CustomSplit, Set.empty, amounts
            let form = {
                Description      = expense.Description
                AmountText       = sprintf "%.2f" expense.PaidAmount
                Currency         = expense.PaidCurrency
                ExchangeRateText = expense.ExchangeRate |> Option.map string |> Option.defaultValue ""
                PaidByIndex      = paidByIndex
                DateText         = sprintf "%04d-%02d-%02d" expense.Date.Year expense.Date.Month expense.Date.Day
                Category         = expense.Category |> Option.defaultValue ""
                Notes            = expense.Notes |> Option.defaultValue ""
                IsSubmitting     = false
                Error            = None
                IsAddingCategory = false
                NewCategoryText  = ""
                SplitMode        = splitMode
                IncludedIndices  = includedIndices
                CustomAmounts    = customAmounts
            }
            { model with Page = AddExpense; ExpenseForm = form; EditingExpenseId = Some expenseId }, Cmd.none

    | DeleteExpenseClick expenseId ->
        match model.ActiveSpaceId with
        | None -> model, Cmd.none
        | Some sid ->
            let confirmed : bool = Fable.Core.JsInterop.emitJsExpr () "window.confirm('Delete this expense?')"
            if not confirmed then model, Cmd.none
            else
                let actorId = resolveActor model
                let cmd =
                    Cmd.OfAsync.either
                        (fun () -> Commands.deleteExpense sid actorId expenseId)
                        ()
                        (fun () -> ExpenseDeleted (Ok ()))
                        (fun ex -> ExpenseDeleted (Error ex.Message))
                model, cmd

    | RecordSettlementClick ->
        let n   = model.SpaceState.Members.Count
        let cur = model.SpaceState.Currency
        { model with Page = RecordSettlement; SettlementForm = emptySettlementForm n cur }, Cmd.none

    | AddCategoryFromForm ->
        match model.ActiveSpaceId with
        | None -> model, Cmd.none
        | Some sid ->
            let name = model.ExpenseForm.NewCategoryText.Trim()
            if name = "" then model, Cmd.none
            elif model.SpaceState.Categories |> Map.containsKey name then
                let form = { model.ExpenseForm with Category = name; IsAddingCategory = false; NewCategoryText = "" }
                { model with ExpenseForm = form }, Cmd.none
            else
                let actorId = resolveActor model
                let cmd =
                    Cmd.OfAsync.either
                        (fun () -> Commands.addCategory sid actorId name)
                        ()
                        (fun () -> CategoryFromFormSaved (Ok name))
                        (fun ex -> CategoryFromFormSaved (Error ex.Message))
                let form = { model.ExpenseForm with IsAddingCategory = false; NewCategoryText = "" }
                { model with ExpenseForm = form; IsAuthLoading = true }, cmd

    | CategoryFromFormSaved (Ok name) ->
        match model.ActiveSpaceId with
        | Some sid ->
            let cmd = Cmd.OfAsync.perform Storage.rebaseAndDisplay sid SpaceRebased
            { model with IsAuthLoading = false; ExpenseForm = { model.ExpenseForm with Category = name } }, cmd
        | None ->
            { model with IsAuthLoading = false }, Cmd.none

    | CategoryFromFormSaved (Error err) ->
        { model with IsAuthLoading = false; ExpenseForm = { model.ExpenseForm with Error = Some err } }, Cmd.none

    | SettlementFromSuggestion s ->
        let members = sortedMembers model.SpaceState
        let findIdx id = members |> List.tryFindIndex (fun m -> m.Id = id) |> Option.defaultValue 0
        let cur = model.SpaceState.Currency
        let form = { emptySettlementForm members.Length cur with
                        FromIndex  = findIdx s.From
                        ToIndex    = findIdx s.To
                        AmountText = sprintf "%.2f" s.Amount }
        { model with Page = RecordSettlement; SettlementForm = form }, Cmd.none

    | ExpenseFormSet form ->
        { model with ExpenseForm = form }, Cmd.none

    | ExpenseSubmit ->
        match model.ActiveSpaceId with
        | None -> model, Cmd.none
        | Some sid ->
            let members  = sortedMembers model.SpaceState
            let form     = model.ExpenseForm
            let paidBy   = members |> List.tryItem form.PaidByIndex |> Option.map (fun m -> m.Id)
            let amtOpt   = try Some (decimal form.AmountText) with _ -> None
            let rateOpt  =
                if form.Currency = model.SpaceState.Currency || form.ExchangeRateText.Trim() = "" then Ok None
                else
                    try Ok (Some (decimal form.ExchangeRateText))
                    with _ -> Error "Invalid exchange rate."
            match paidBy, amtOpt, rateOpt with
            | _, _, Error err ->
                { model with ExpenseForm = { form with Error = Some err } }, Cmd.none
            | Some paidById, Some amount, Ok rateOpt' ->
                if amount <= 0m then
                    { model with ExpenseForm = { form with Error = Some "Amount must be positive." } }, Cmd.none
                elif form.Currency <> model.SpaceState.Currency && rateOpt' = None then
                    { model with ExpenseForm = { form with Error = Some "Exchange rate required for foreign currency." } }, Cmd.none
                elif form.Description.Trim() = "" then
                    { model with ExpenseForm = { form with Error = Some "Description is required." } }, Cmd.none
                else
                    let actorId = resolveActor model
                    let split =
                        match form.SplitMode with
                        | EqualSplit ->
                            form.IncludedIndices
                            |> Set.toList
                            |> List.choose (fun i -> members |> List.tryItem i |> Option.map (fun m -> m.Id))
                            |> Equal
                        | CustomSplit ->
                            form.CustomAmounts
                            |> Map.toList
                            |> List.choose (fun (i, txt) ->
                                try
                                    let amt = decimal txt
                                    if amt > 0m then members |> List.tryItem i |> Option.map (fun m -> m.Id, amt)
                                    else None
                                with _ -> None)
                            |> Exact
                    let date     = parseFormDate form.DateText
                    let category = if form.Category = "" then None else Some form.Category
                    let notes    = if form.Notes.Trim() = "" then None else Some (form.Notes.Trim())
                    let cmd =
                        match model.EditingExpenseId with
                        | Some expId ->
                            Cmd.OfAsync.either
                                (fun () -> Commands.correctExpense sid actorId expId form.Description amount form.Currency rateOpt' paidById split date category notes)
                                ()
                                (fun () -> ExpenseCorrected (Ok ()))
                                (fun ex  -> ExpenseCorrected (Error ex.Message))
                        | None ->
                            Cmd.OfAsync.either
                                (fun () -> Commands.addExpense sid actorId form.Description amount form.Currency rateOpt' paidById split date category notes)
                                ()
                                (fun () -> ExpenseSaved (Ok ()))
                                (fun ex  -> ExpenseSaved (Error ex.Message))
                    { model with ExpenseForm = { form with IsSubmitting = true; Error = None } }, cmd
            | _, None, _ ->
                { model with ExpenseForm = { form with Error = Some "Invalid amount." } }, Cmd.none
            | None, _, _ ->
                { model with ExpenseForm = { form with Error = Some "Please select who paid." } }, Cmd.none

    | ExpenseSaved (Ok ()) ->
        match model.ActiveSpaceId with
        | Some sid ->
            let loadCmd  = Cmd.OfAsync.perform Storage.loadSpaceState sid (fun ss -> SpaceLoaded (sid, ss))
            let clearCmd = Cmd.OfAsync.perform (fun () -> Async.Sleep Styles.toastDurationMs) () (fun () -> ToastCleared)
            { model with ExpenseForm = { model.ExpenseForm with IsSubmitting = false }; Toast = Some "Expense saved!" },
            Cmd.batch [ loadCmd; clearCmd ]
        | None ->
            { model with Page = SpaceOverview }, Cmd.none

    | ExpenseSaved (Error err) ->
        { model with ExpenseForm = { model.ExpenseForm with IsSubmitting = false; Error = Some err } }, Cmd.none

    | ExpenseCorrected (Ok ()) ->
        match model.ActiveSpaceId with
        | Some sid ->
            let loadCmd  = Cmd.OfAsync.perform Storage.loadSpaceState sid (fun ss -> SpaceLoaded (sid, ss))
            let clearCmd = Cmd.OfAsync.perform (fun () -> Async.Sleep Styles.toastDurationMs) () (fun () -> ToastCleared)
            { model with ExpenseForm = { model.ExpenseForm with IsSubmitting = false }; EditingExpenseId = None; Toast = Some "Expense updated!" },
            Cmd.batch [ loadCmd; clearCmd ]
        | None ->
            { model with Page = SpaceOverview }, Cmd.none

    | ExpenseCorrected (Error err) ->
        { model with ExpenseForm = { model.ExpenseForm with IsSubmitting = false; Error = Some err } }, Cmd.none

    | ExpenseDeleted (Ok ()) ->
        match model.ActiveSpaceId with
        | Some sid ->
            let loadCmd  = Cmd.OfAsync.perform Storage.loadSpaceState sid (fun ss -> SpaceLoaded (sid, ss))
            let clearCmd = Cmd.OfAsync.perform (fun () -> Async.Sleep Styles.toastDurationMs) () (fun () -> ToastCleared)
            { model with Toast = Some "Expense deleted!" }, Cmd.batch [ loadCmd; clearCmd ]
        | None -> model, Cmd.none

    | ExpenseDeleted (Error err) ->
        { model with Toast = Some $"Delete failed: %s{err}" }, Cmd.none

    | SettlementFormSet form ->
        { model with SettlementForm = form }, Cmd.none

    | SettlementSubmit ->
        match model.ActiveSpaceId with
        | None -> model, Cmd.none
        | Some sid ->
            let members   = sortedMembers model.SpaceState
            let form      = model.SettlementForm
            let groupCur  = model.SpaceState.Currency
            let fromOpt   = members |> List.tryItem form.FromIndex |> Option.map (fun m -> m.Id)
            let toOpt     = members |> List.tryItem form.ToIndex   |> Option.map (fun m -> m.Id)
            let amtOpt    = try Some (decimal form.AmountText) with _ -> None
            let rateOpt   =
                if form.Currency = groupCur || form.ExchangeRateText.Trim() = "" then Ok None
                else try Ok (Some (decimal form.ExchangeRateText)) with _ -> Error "Invalid exchange rate."
            let amt2Opt   =
                if not form.UseSecondPayment || form.AmountText2.Trim() = "" then Ok None
                else try Ok (Some (decimal form.AmountText2)) with _ -> Error "Invalid second amount."
            let rate2Opt  =
                if not form.UseSecondPayment || form.Currency2 = groupCur || form.ExchangeRateText2.Trim() = "" then Ok None
                else try Ok (Some (decimal form.ExchangeRateText2)) with _ -> Error "Invalid second exchange rate."
            match fromOpt, toOpt, amtOpt, rateOpt, amt2Opt, rate2Opt with
            | _, _, _, Error err, _, _ | _, _, _, _, _, Error err | _, _, _, _, Error err, _ ->
                { model with SettlementForm = { form with Error = Some err } }, Cmd.none
            | Some fromId, Some toId, Some amount, Ok rate, Ok amt2, Ok rate2 ->
                if fromId = toId then
                    { model with SettlementForm = { form with Error = Some "From and To must be different members." } }, Cmd.none
                elif amount <= 0m then
                    { model with SettlementForm = { form with Error = Some "Amount must be positive." } }, Cmd.none
                elif form.Currency <> groupCur && rate = None then
                    { model with SettlementForm = { form with Error = Some "Exchange rate required for foreign currency." } }, Cmd.none
                elif form.UseSecondPayment && form.Currency2 <> groupCur && rate2 = None then
                    { model with SettlementForm = { form with Error = Some "Exchange rate required for second currency." } }, Cmd.none
                else
                    let actorId = resolveActor model
                    let date    = parseFormDate form.DateText
                    let notes   = if form.Notes.Trim() = "" then None else Some (form.Notes.Trim())
                    let save1 = Commands.recordSettlement sid actorId fromId toId amount form.Currency rate date notes
                    let save2 =
                        match form.UseSecondPayment, amt2 with
                        | true, Some a2 when a2 > 0m ->
                            Commands.recordSettlement sid actorId fromId toId a2 form.Currency2 rate2 date notes
                        | _ -> async { return () }
                    let saveAll () = async {
                        do! save1
                        do! save2
                    }
                    let cmd =
                        Cmd.OfAsync.either saveAll ()
                            (fun () -> SettlementSaved (Ok ()))
                            (fun ex  -> SettlementSaved (Error ex.Message))
                    { model with SettlementForm = { form with IsSubmitting = true; Error = None } }, cmd
            | _, _, None, _, _, _ ->
                { model with SettlementForm = { form with Error = Some "Invalid amount." } }, Cmd.none
            | _ ->
                { model with SettlementForm = { form with Error = Some "Please select both members." } }, Cmd.none

    | SettlementSaved (Ok ()) ->
        match model.ActiveSpaceId with
        | Some sid ->
            let loadCmd  = Cmd.OfAsync.perform Storage.loadSpaceState sid (fun ss -> SpaceLoaded (sid, ss))
            let clearCmd = Cmd.OfAsync.perform (fun () -> Async.Sleep Styles.toastDurationMs) () (fun () -> ToastCleared)
            { model with SettlementForm = { model.SettlementForm with IsSubmitting = false }; Toast = Some "Settlement recorded!" },
            Cmd.batch [ loadCmd; clearCmd ]
        | None ->
            { model with Page = SpaceOverview }, Cmd.none

    | SettlementSaved (Error err) ->
        { model with SettlementForm = { model.SettlementForm with IsSubmitting = false; Error = Some err } }, Cmd.none

    | SpaceStateUpdated ss ->
        { model with SpaceState = ss }, Cmd.none

    | RemoteEventReceived sid ->
        match model.ActiveSpaceId with
        | Some active when active = sid ->
            let cmd = Cmd.OfAsync.perform Storage.rebaseAndDisplay sid SpaceRebased
            model, cmd
        | _ -> model, Cmd.none

    | SpaceRebased (state, newConflicts) ->
        let allConflicts = model.Conflicts @ newConflicts
        let toast =
            if newConflicts.IsEmpty then model.Toast
            else
                let summary = newConflicts |> List.map (fun c -> sprintf "%s: %s" c.Description c.Reason) |> String.concat "\n"
                Some (sprintf "Some changes could not be applied:\n%s" summary)
        { model with SpaceState = state; Conflicts = allConflicts; Toast = toast }, Cmd.none

    | SyncOutcome (sid, outcomes) ->
        let needsRebase = outcomes |> List.exists (fun o -> o = Sync.PermanentRejection)
        let cmd =
            if needsRebase then Cmd.OfAsync.perform Storage.rebaseAndDisplay sid SpaceRebased
            else Cmd.none
        model, cmd

    | FlushPending ->
        match model.ActiveSpaceId, model.Auth with
        | Some sid, Some user ->
            let cmd = Cmd.OfAsync.perform (fun () -> Sync.pushPending user.AccessToken) () (fun outcomes -> SyncOutcome (sid, outcomes))
            model, cmd
        | _ -> model, Cmd.none

    | DismissConflict eventId ->
        let (EventId g) = eventId
        let cmd = Cmd.OfAsync.attempt (fun () -> IndexedDb.deleteEvent (string g)) () (fun _ -> SyncDone)
        { model with Conflicts = model.Conflicts |> List.filter (fun c -> c.EventId <> eventId) }, cmd

    | SyncDone ->
        model, Cmd.none

    | ToastCleared ->
        { model with Toast = None }, Cmd.none

    | SettingsToggled ->
        if model.ShowSettings then
            { model with ShowSettings = false
                         IsEditingSpaceName = false; SpaceNameText = ""
                         EditingCategory = None; EditCategoryName = "" }, Cmd.none
        else
            { model with ShowSettings = true }, Cmd.none

    | StartSpaceRename ->
        { model with IsEditingSpaceName = true; SpaceNameText = model.SpaceState.Name }, Cmd.none

    | SpaceNameTextSet text ->
        { model with SpaceNameText = text }, Cmd.none

    | SaveSpaceRename ->
        match model.ActiveSpaceId with
        | None -> model, Cmd.none
        | Some sid ->
            let name = model.SpaceNameText.Trim()
            if name = "" || name = model.SpaceState.Name then
                { model with IsEditingSpaceName = false }, Cmd.none
            else
                let actorId = resolveActor model
                let cmd =
                    Cmd.OfAsync.either
                        (fun () -> Commands.renameSpace sid actorId name)
                        ()
                        (fun () -> SpaceNameSaved (Ok ()))
                        (fun ex -> SpaceNameSaved (Error ex.Message))
                model, cmd

    | SpaceNameSaved (Ok ()) ->
        match model.ActiveSpaceId with
        | Some sid ->
            let cmd = Cmd.OfAsync.perform Storage.loadSpaceState sid (fun ss -> SpaceLoaded (sid, ss))
            { model with IsEditingSpaceName = false; SpaceNameText = "" }, cmd
        | None -> { model with IsEditingSpaceName = false }, Cmd.none

    | SpaceNameSaved (Error _) ->
        { model with IsEditingSpaceName = false }, Cmd.none

    | StartProfileRename ->
        let name = resolveActor model
                   |> fun id -> model.SpaceState.Members |> Map.tryFind id
                   |> Option.map _.DisplayName
                   |> Option.defaultValue ""
        { model with IsEditingProfileName = true; ProfileNameText = name; ProfileNameError = None }, Cmd.none

    | ProfileNameTextSet text ->
        { model with ProfileNameText = text; ProfileNameError = None }, Cmd.none

    | SaveProfileRename ->
        match model.ActiveSpaceId with
        | None -> model, Cmd.none
        | Some sid ->
            let name = model.ProfileNameText.Trim()
            if name = "" then
                { model with ProfileNameError = Some "Name is required." }, Cmd.none
            else
                let actorId = resolveActor model
                let cmd =
                    Cmd.OfAsync.either
                        (fun () -> Commands.renameMember sid actorId actorId name)
                        ()
                        (fun () -> ProfileNameSaved (Ok ()))
                        (fun ex -> ProfileNameSaved (Error ex.Message))
                { model with ProfileNameError = None }, cmd

    | ProfileNameSaved (Ok ()) ->
        match model.ActiveSpaceId with
        | Some sid ->
            let cmd = Cmd.OfAsync.perform Storage.loadSpaceState sid (fun ss -> SpaceLoaded (sid, ss))
            { model with IsEditingProfileName = false; ProfileNameText = "" }, cmd
        | None -> { model with IsEditingProfileName = false }, Cmd.none

    | ProfileNameSaved (Error err) ->
        { model with ProfileNameError = Some err }, Cmd.none

    | SpaceSwitcherToggled ->
        { model with ShowSpaceSwitcher = not model.ShowSpaceSwitcher }, Cmd.none

    | SwitchToSpace sid ->
        if model.ActiveSpaceId = Some sid then
            { model with ShowSpaceSwitcher = false }, Cmd.none
        else
            setActiveSpaceId sid
            let cmd = Cmd.OfAsync.perform Storage.loadSpaceState sid (fun ss -> SpaceLoaded (sid, ss))
            { model with ShowSpaceSwitcher = false; ActiveSpaceId = Some sid; Page = SpaceOverview }, cmd

    | DeleteSpaceClick ->
        match model.ActiveSpaceId with
        | None -> model, Cmd.none
        | Some sid ->
            let confirmed : bool = emitJsExpr () "window.confirm('Delete this space? All data will be permanently lost.')"
            if not confirmed then model, Cmd.none
            else
                let cmd =
                    Cmd.OfAsync.either
                        (fun () -> Storage.deleteSpace sid)
                        ()
                        (fun () -> SpaceDeleted (Ok ()))
                        (fun ex  -> SpaceDeleted (Error ex.Message))
                model, cmd

    | SpaceDeleted (Ok ()) ->
        let remaining = Storage.getKnownSpaces ()
        match remaining with
        | next :: _ ->
            setActiveSpaceId next.Id
            let cmd = Cmd.OfAsync.perform Storage.loadSpaceState next.Id (fun ss -> SpaceLoaded (next.Id, ss))
            { model with ShowSettings = false; KnownSpaces = remaining; ActiveSpaceId = Some next.Id }, cmd
        | [] ->
            clearActiveSpaceId ()
            { model with ShowSettings = false; KnownSpaces = []
                         ActiveSpaceId = None; SpaceState = SpaceState.Empty; Page = CreateSpace }, Cmd.none

    | SpaceDeleted (Error err) ->
        { model with Toast = Some $"Delete failed: %s{err}" }, Cmd.none

    | CreateSpaceNameSet text ->
        { model with CreateSpaceForm = { model.CreateSpaceForm with SpaceNameText = text; Error = None } }, Cmd.none

    | CreateSpaceCurrencySet text ->
        { model with CreateSpaceForm = { model.CreateSpaceForm with CurrencyText = text; Error = None } }, Cmd.none

    | CreateSpaceMemberSet text ->
        { model with CreateSpaceForm = { model.CreateSpaceForm with MemberName = text; Error = None } }, Cmd.none

    | CreateSpaceSubmit ->
        let form = model.CreateSpaceForm
        let name = form.SpaceNameText.Trim()
        let cur  = form.CurrencyText.Trim().ToUpper()
        let mem  = form.MemberName.Trim()
        if name = "" then
            { model with CreateSpaceForm = { form with Error = Some "Space name is required." } }, Cmd.none
        elif cur = "" then
            { model with CreateSpaceForm = { form with Error = Some "Currency is required." } }, Cmd.none
        elif mem = "" then
            { model with CreateSpaceForm = { form with Error = Some "Your name is required." } }, Cmd.none
        else
            let userId = model.Auth |> Option.map (fun u -> UserId (System.Guid.Parse u.Id))
            let cmd =
                Cmd.OfAsync.either
                    (fun () -> Commands.createSpace name cur mem userId)
                    ()
                    (fun sid -> CreateSpaceDone (Ok sid))
                    (fun ex  -> CreateSpaceDone (Error ex.Message))
            { model with CreateSpaceForm = { form with IsSubmitting = true; Error = None } }, cmd

    | CreateSpaceDone (Ok sid) ->
        let name = model.CreateSpaceForm.SpaceNameText.Trim()
        Storage.upsertKnownSpace sid name
        setActiveSpaceId sid
        let cmd = Cmd.OfAsync.perform Storage.loadSpaceState sid (fun ss -> SpaceLoaded (sid, ss))
        { model with CreateSpaceForm = emptyCreateSpaceForm; ActiveSpaceId = Some sid }, cmd

    | CreateSpaceDone (Error err) ->
        { model with CreateSpaceForm = { model.CreateSpaceForm with IsSubmitting = false; Error = Some err } }, Cmd.none

#if DEVMODE
    | DevActorSet memberId ->
        setDevActorId memberId
        { model with DevActorId = Some memberId }, Cmd.none

    | DevReset ->
        let cmd =
            Cmd.OfAsync.attempt
                (fun () -> async {
                    do! IndexedDb.clearAllEvents ()
                    browserStorage?removeItem("activeSpaceId") |> ignore
                    sessionStore?removeItem("devActorId") |> ignore
                    emitJsExpr<unit> () "window.location.reload()"
                })
                ()
                (fun _ -> NavigateTo DevBootstrap)
        model, cmd
#endif

let subscribe (model: Model) : Sub<Msg> =
    let authSub (dispatch: Msg -> unit) =
        let unsub = Auth.subscribe (AuthReceived >> dispatch)
        { new System.IDisposable with member _.Dispose() = unsub () }

    let onlineSub (dispatch: Msg -> unit) =
        let handler : obj = System.Func<obj, unit>(fun _ -> dispatch FlushPending) |> box
        emitJsExpr (handler) "window.addEventListener('online', $0)" |> ignore
        { new System.IDisposable with
            member _.Dispose() = emitJsExpr (handler) "window.removeEventListener('online', $0)" |> ignore }

    let baseSubs = [ ["auth"], authSub; ["online"], onlineSub ]

    match model.ActiveSpaceId with
    | None -> baseSubs
    | Some sid ->
        let (SpaceId g) = sid
#if DEVMODE
        if DevMode.isEnabled () then
            let devBroadcastSub (dispatch: Msg -> unit) =
                let channel : obj = emitJsExpr () "new BroadcastChannel('splittea-dev')"
                channel?addEventListener("message", fun (e: obj) ->
                    try dispatch (RemoteEventReceived (SpaceId (System.Guid.Parse (string e?data))))
                    with _ -> ()) |> ignore
                { new System.IDisposable with member _.Dispose() = channel?close() |> ignore }
            baseSubs @ [ ["dev-broadcast"; string g], devBroadcastSub ]
        else
#endif
        let realtimeSub (dispatch: Msg -> unit) =
            let unsub = Sync.subscribeSpace sid (fun () -> dispatch (RemoteEventReceived sid))
            { new System.IDisposable with member _.Dispose() = unsub () }
        baseSubs @ [ ["realtime"; string g], realtimeSub ]

let private signInView (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.className "min-h-screen bg-gray-50 flex items-center justify-center px-4"
        prop.children [
            Html.div [
                prop.className "w-full max-w-sm space-y-6"
                prop.children [
                    Html.h1 [ prop.className "text-3xl font-bold text-center text-teal-700"; prop.text "SplitTea" ]
                    Html.div [
                        prop.className "bg-white rounded-xl shadow-sm border border-gray-200 p-6 space-y-4"
                        prop.children [
                            Html.label [
                                prop.className "block text-sm font-medium text-gray-700"
                                prop.text "Email"
                                prop.htmlFor "email"
                            ]
                            Html.input [
                                prop.id "email"
                                prop.type' "email"
                                prop.className "w-full border border-gray-300 rounded-lg px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-teal-500"
                                prop.placeholder "you@example.com"
                                prop.value model.SignInEmail
                                prop.onChange (SignInEmailSet >> dispatch)
                                prop.onKeyDown (fun e -> if e.key = "Enter" then dispatch SignInSubmit)
                            ]
                            match model.SignInError with
                            | Some msg -> Html.p [ prop.className "text-sm text-gray-600"; prop.text msg ]
                            | None     -> ()
                            Html.button [
                                prop.className "w-full bg-teal-600 hover:bg-teal-700 disabled:opacity-50 text-white font-semibold py-2 rounded-lg transition-colors"
                                prop.disabled model.IsAuthLoading
                                prop.text (if model.IsAuthLoading then "Sending..." else "Send Magic Link")
                                prop.onClick (fun _ -> dispatch SignInSubmit)
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

#if DEVMODE
let private devBootstrapView (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.className "min-h-screen bg-gray-50 flex items-center justify-center px-4"
        prop.children [
            Html.div [
                prop.className "w-full max-w-sm space-y-6"
                prop.children [
                    Html.h1 [ prop.className "text-3xl font-bold text-center text-teal-700"; prop.text "SplitTea" ]
                    Html.div [
                        prop.className "bg-white rounded-xl shadow-sm border border-gray-200 p-6 space-y-4"
                        prop.children [
                            Html.p [
                                prop.className "text-sm text-gray-600"
                                prop.text "Dev mode is active. Create a local-only test space stored in IndexedDB."
                            ]
                            Html.button [
                                prop.className "w-full bg-teal-600 hover:bg-teal-700 disabled:opacity-50 text-white font-semibold py-2 rounded-lg transition-colors"
                                prop.disabled model.IsAuthLoading
                                prop.text (if model.IsAuthLoading then "Creating..." else "Create Local Test Space")
                                prop.onClick (fun _ -> dispatch CreateDevSpace)
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
#endif

let private loadingView () =
    Html.div [
        prop.className "min-h-screen bg-gray-50 flex items-center justify-center"
        prop.children [
            Html.p [ prop.className "text-gray-500 text-sm"; prop.text "Loading..." ]
        ]
    ]

#if DEVMODE
let private devActorBadge (model: Model) (dispatch: Msg -> unit) =
    let members =
        model.SpaceState.Members
        |> Map.toList
        |> List.sortBy (fun (_, m) -> m.DisplayName)
    Html.div [
        prop.className "fixed bottom-24 right-4 z-50 bg-gray-800 text-white text-xs rounded-lg px-3 py-2 shadow-lg flex items-center gap-2"
        prop.children [
            Html.span [ prop.className "text-gray-400"; prop.text "Acting as" ]
            Html.select [
                prop.className "bg-gray-700 text-white text-xs rounded px-1.5 py-0.5 focus:outline-none"
                prop.value (
                    model.DevActorId
                    |> Option.map (fun (MemberId g) -> string g)
                    |> Option.defaultValue "")
                prop.onChange (fun (v: string) ->
                    try dispatch (DevActorSet (MemberId (System.Guid.Parse v)))
                    with _ -> ())
                prop.children (
                    members |> List.map (fun (MemberId g, m) ->
                        Html.option [ prop.value (string g); prop.text m.DisplayName ]))
            ]
            Html.span [ prop.className "text-gray-600"; prop.text "|" ]
            Html.button [
                prop.type' "button"
                prop.className "text-red-400 hover:text-red-300 transition-colors"
                prop.title "Clear all data and restart"
                prop.text "↺ Reset"
                prop.onClick (fun _ -> dispatch DevReset)
            ]
        ]
    ]
#endif

let private spaceSwitcherSheet (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.className "fixed inset-0 z-50 flex items-end sm:items-center justify-center"
        prop.children [
            Html.div [
                prop.className "absolute inset-0 bg-black/40"
                prop.onClick (fun _ -> dispatch SpaceSwitcherToggled)
            ]
            Html.div [
                prop.className "relative z-10 bg-white rounded-t-2xl sm:rounded-2xl w-full sm:max-w-md p-5 space-y-3"
                prop.children [
                    Html.div [
                        prop.className "flex items-center justify-between"
                        prop.children [
                            Html.h2 [ prop.className "text-lg font-bold text-gray-900"; prop.text "Spaces" ]
                            Html.button [
                                prop.type' "button"
                                prop.className "p-1 text-gray-400 hover:text-gray-600 rounded-lg transition-colors text-lg leading-none"
                                prop.onClick (fun _ -> dispatch SpaceSwitcherToggled)
                                prop.text "✕"
                            ]
                        ]
                    ]
                    Html.div [
                        prop.className "space-y-1"
                        prop.children (
                            model.KnownSpaces |> List.map (fun s ->
                                let isActive = Some s.Id = model.ActiveSpaceId
                                Html.button [
                                    prop.type' "button"
                                    prop.className (
                                        Styles.cx [
                                            "w-full flex items-center gap-3 px-3 py-2.5 rounded-lg text-left text-sm transition-colors"
                                            if isActive then "bg-teal-50 text-teal-700 font-medium"
                                            else "text-gray-700 hover:bg-gray-50"
                                        ])
                                    prop.onClick (fun _ -> dispatch (SwitchToSpace s.Id))
                                    prop.children [
                                        Html.span [ prop.className "flex-1"; prop.text (if s.Name = "" then "(Loading...)" else s.Name) ]
                                        if isActive then Icons.check
                                    ]
                                ])
                        )
                    ]
                    Html.button [
                        prop.type' "button"
                        prop.className "w-full flex items-center gap-2 px-3 py-2.5 rounded-lg text-sm text-teal-600 hover:bg-teal-50 font-medium transition-colors"
                        prop.onClick (fun _ ->
                            dispatch SpaceSwitcherToggled
                            dispatch (NavigateTo CreateSpace))
                        prop.children [
                            Icons.plus
                            Html.span [ prop.text "Create new space" ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let private navTab (label: string) (icon: ReactElement) (active: bool) (onClick: unit -> unit) =
    Html.button [
        prop.type' "button"
        prop.className (
            Styles.cx [
                "flex-1 flex flex-col items-center gap-1 py-2 text-xs font-medium transition-colors"
                if active then "text-teal-600" else "text-gray-400 hover:text-gray-600"
            ])
        prop.onClick (fun _ -> onClick ())
        prop.children [
            icon
            Html.span [ prop.text label ]
        ]
    ]


let private bottomNav (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.className "fixed bottom-0 inset-x-0 bg-white border-t border-gray-200 z-40"
        prop.children [
            Html.div [
                prop.className "max-w-lg mx-auto flex"
                prop.children [
                    navTab "Home"      Icons.home   (model.Page = SpaceOverview) (fun () -> dispatch (NavigateTo SpaceOverview))
                    navTab "Analytics" Icons.chart (model.Page = Analytics)     (fun () -> dispatch (NavigateTo Analytics))
                    navTab "Profile"   Icons.person (model.Page = Profile)      (fun () -> dispatch (NavigateTo Profile))
                ]
            ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    let isSpaceTab = model.Page = SpaceOverview || model.Page = Analytics || model.Page = Profile

    let currentMember =
        if isSpaceTab then
            let actorId = resolveActor model
            model.SpaceState.Members |> Map.tryFind actorId
        else None

    let inner =
        match model.Page with
        | Loading             -> loadingView ()
        | SignIn              -> signInView model dispatch
#if DEVMODE
        | DevBootstrap        -> devBootstrapView model dispatch
#endif
        | SpaceOverview       -> SpacePage.view model.SpaceState model dispatch
        | Analytics           -> AnalyticsPage.view model.SpaceState
        | Profile             ->
            let displayName = currentMember |> Option.map _.DisplayName |> Option.defaultValue ""
            let email       = model.Auth |> Option.bind _.Email
            ProfilePage.view displayName email model dispatch
        | AddExpense          -> ExpenseFormPage.view model.SpaceState model.ExchangeRates model.ExpenseForm model.EditingExpenseId.IsSome dispatch
        | RecordSettlement    -> SettlementFormPage.view model.SpaceState model.ExchangeRates model.SettlementForm dispatch
        | CreateSpace         -> CreateSpacePage.view model.ActiveSpaceId.IsSome model dispatch

    let toast =
        match model.Toast with
        | Some msg ->
            Html.div [
                prop.className "fixed top-4 inset-x-0 flex justify-center z-50 pointer-events-none"
                prop.children [
                    Html.div [
                        prop.className "bg-gray-800 text-white text-sm font-medium px-4 py-2.5 rounded-full shadow-lg"
                        prop.text msg
                    ]
                ]
            ]
        | None -> Html.none

    let nav      = if isSpaceTab then bottomNav model dispatch else Html.none
    let switcher = if model.ShowSpaceSwitcher then spaceSwitcherSheet model dispatch else Html.none

#if DEVMODE
    if DevMode.isEnabled () && not model.SpaceState.Members.IsEmpty then
        Html.div [ prop.children [ inner; nav; devActorBadge model dispatch; switcher; toast ] ]
    else
        Html.div [ prop.children [ inner; nav; switcher; toast ] ]
#else
    Html.div [ prop.children [ inner; nav; switcher; toast ] ]
#endif
