module AppRoot

open Elmish
open Feliz
open Fable.Core
open Fable.Core.JsInterop
open SplitTea.Core
open UITypes

let private localStorage : obj = emitJsExpr () "localStorage"

let private getActiveGroupId () : GroupId option =
    let v : obj = localStorage?getItem("activeGroupId")
    if isNull v then None
    else
        try Some (GroupId (System.Guid.Parse (string v)))
        with _ -> None

let private setActiveGroupId (id: GroupId) =
    let (GroupId g) = id
    localStorage?setItem("activeGroupId", string g) |> ignore

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

let private sortedMembers (state: GroupState) =
    state.Members
    |> Map.toList
    |> List.map snd
    |> List.sortBy (fun m -> m.DisplayName)

let private findActorId (state: GroupState) (user: Auth.AuthUser option) : MemberId =
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

let private emptyContextForm () : ContextForm = {
    Name         = ""
    Template     = Trip
    DateFromText = ""
    DateToText   = ""
    IsSubmitting = false
    Error        = None
}

let private emptyExpenseForm (groupCurrency: string) : ExpenseForm = {
    Description      = ""
    AmountText       = ""
    Currency         = groupCurrency
    ExchangeRateText = ""
    PaidByIndex      = 0
    DateText         = todayStr ()
    Category         = ""
    Notes            = ""
    ContextIndex     = 0
    IsSubmitting     = false
    Error            = None
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
    let activeGroupId = getActiveGroupId ()
    let model = {
        Auth           = None
        Page           = Loading
        ActiveGroupId  = activeGroupId
        GroupState     = GroupState.Empty
        ExchangeRates  = Map.empty
        SignInEmail    = ""
        SignInError    = None
        IsAuthLoading  = true
        ContextForm    = emptyContextForm ()
        ExpenseForm    = emptyExpenseForm ""
        SettlementForm = emptySettlementForm 0 ""
    }
    let cmd =
        match activeGroupId with
        | Some gid -> Cmd.OfAsync.perform Storage.loadGroupState gid (fun gs -> GroupLoaded (gid, gs))
        | None     -> Cmd.none
    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | AuthReceived (Auth.SignedIn user) ->
        let page =
            match model.Page with
            | Loading -> if model.ActiveGroupId.IsSome then Loading else SignIn
            | p -> p
        { model with Auth = Some user; IsAuthLoading = false; Page = page }, Cmd.none

    | AuthReceived Auth.SignedOut ->
        { model with Auth = None; IsAuthLoading = false; Page = SignIn }, Cmd.none

    | SignInEmailSet email ->
        { model with SignInEmail = email; SignInError = None }, Cmd.none

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

    | GroupLoaded (gid, gs) ->
        setActiveGroupId gid
        let syncCmd = Cmd.OfAsync.attempt (fun () -> Sync.pushPending ()) () (fun _ -> SyncDone)
        let fxCmd   =
            Cmd.OfAsync.either
                (fun () -> FxRates.getRates gs.Currency)
                ()
                ExchangeRatesLoaded
                (fun _ -> ExchangeRatesLoaded Map.empty)
        { model with ActiveGroupId = Some gid; GroupState = gs; Page = GroupOverview }, Cmd.batch [ syncCmd; fxCmd ]

    | ExchangeRatesLoaded rates ->
        { model with ExchangeRates = rates }, Cmd.none

    | CreateContextClick ->
        { model with Page = CreateContext; ContextForm = emptyContextForm () }, Cmd.none

    | ContextFormSet form ->
        { model with ContextForm = form }, Cmd.none

    | ContextSubmit ->
        match model.ActiveGroupId with
        | None -> model, Cmd.none
        | Some gid ->
            let form = model.ContextForm
            if form.Name.Trim() = "" then
                { model with ContextForm = { form with Error = Some "Name is required." } }, Cmd.none
            else
                let actorId  = findActorId model.GroupState model.Auth
                let dateFrom = if form.DateFromText = "" then None else try Some (parseFormDate form.DateFromText) with _ -> None
                let dateTo   = if form.DateToText   = "" then None else try Some (parseFormDate form.DateToText)   with _ -> None
                let cmd =
                    Cmd.OfAsync.either
                        (fun () -> Commands.createContext gid actorId (form.Name.Trim()) form.Template None dateFrom dateTo)
                        ()
                        (fun () -> ContextSaved (Ok ()))
                        (fun ex  -> ContextSaved (Error ex.Message))
                { model with ContextForm = { form with IsSubmitting = true; Error = None } }, cmd

    | ContextSaved (Ok ()) ->
        match model.ActiveGroupId with
        | Some gid ->
            let cmd = Cmd.OfAsync.perform Storage.loadGroupState gid (fun gs -> GroupLoaded (gid, gs))
            { model with ContextForm = { model.ContextForm with IsSubmitting = false } }, cmd
        | None ->
            { model with Page = GroupOverview }, Cmd.none

    | ContextSaved (Error err) ->
        { model with ContextForm = { model.ContextForm with IsSubmitting = false; Error = Some err } }, Cmd.none

    | OpenContext contextId ->
        { model with Page = ContextDetail contextId }, Cmd.none

    | GroupNotFound ->
        { model with Page = SignIn }, Cmd.none

    | NavigateTo page ->
        { model with Page = page }, Cmd.none

    | AddExpenseClick ->
        let cur = model.GroupState.Currency
        { model with Page = AddExpense; ExpenseForm = emptyExpenseForm cur }, Cmd.none

    | RecordSettlementClick ->
        let n   = model.GroupState.Members.Count
        let cur = model.GroupState.Currency
        { model with Page = RecordSettlement; SettlementForm = emptySettlementForm n cur }, Cmd.none

    | ExpenseFormSet form ->
        { model with ExpenseForm = form }, Cmd.none

    | ExpenseSubmit ->
        match model.ActiveGroupId with
        | None -> model, Cmd.none
        | Some gid ->
            let members  = sortedMembers model.GroupState
            let form     = model.ExpenseForm
            let paidBy   = members |> List.tryItem form.PaidByIndex |> Option.map (fun m -> m.Id)
            let amtOpt   = try Some (decimal form.AmountText) with _ -> None
            let rateOpt  =
                if form.Currency = model.GroupState.Currency || form.ExchangeRateText.Trim() = "" then Ok None
                else
                    try Ok (Some (decimal form.ExchangeRateText))
                    with _ -> Error "Invalid exchange rate."
            match paidBy, amtOpt, rateOpt with
            | _, _, Error err ->
                { model with ExpenseForm = { form with Error = Some err } }, Cmd.none
            | Some paidById, Some amount, Ok rateOpt' ->
                if amount <= 0m then
                    { model with ExpenseForm = { form with Error = Some "Amount must be positive." } }, Cmd.none
                elif form.Currency <> model.GroupState.Currency && rateOpt' = None then
                    { model with ExpenseForm = { form with Error = Some "Exchange rate required for foreign currency." } }, Cmd.none
                elif form.Description.Trim() = "" then
                    { model with ExpenseForm = { form with Error = Some "Description is required." } }, Cmd.none
                else
                    let actorId  = findActorId model.GroupState model.Auth
                    let split    = Equal (members |> List.map (fun m -> m.Id))
                    let date     = parseFormDate form.DateText
                    let category = if form.Category = "" then None else Some form.Category
                    let notes    = if form.Notes.Trim() = "" then None else Some (form.Notes.Trim())
                    let sortedContexts = model.GroupState.Contexts |> Map.toList |> List.map snd |> List.sortBy (fun c -> c.Name)
                    let contextId =
                        if form.ContextIndex = 0 then None
                        else sortedContexts |> List.tryItem (form.ContextIndex - 1) |> Option.map (fun c -> c.ContextId)
                    let cmd =
                        Cmd.OfAsync.either
                            (fun () -> Commands.addExpense gid actorId form.Description amount form.Currency rateOpt' paidById split date category notes contextId)
                            ()
                            (fun () -> ExpenseSaved (Ok ()))
                            (fun ex  -> ExpenseSaved (Error ex.Message))
                    { model with ExpenseForm = { form with IsSubmitting = true; Error = None } }, cmd
            | _, None, _ ->
                { model with ExpenseForm = { form with Error = Some "Invalid amount." } }, Cmd.none
            | None, _, _ ->
                { model with ExpenseForm = { form with Error = Some "Please select who paid." } }, Cmd.none

    | ExpenseSaved (Ok ()) ->
        match model.ActiveGroupId with
        | Some gid ->
            let cmd = Cmd.OfAsync.perform Storage.loadGroupState gid (fun gs -> GroupLoaded (gid, gs))
            { model with ExpenseForm = { model.ExpenseForm with IsSubmitting = false } }, cmd
        | None ->
            { model with Page = GroupOverview }, Cmd.none

    | ExpenseSaved (Error err) ->
        { model with ExpenseForm = { model.ExpenseForm with IsSubmitting = false; Error = Some err } }, Cmd.none

    | SettlementFormSet form ->
        { model with SettlementForm = form }, Cmd.none

    | SettlementSubmit ->
        match model.ActiveGroupId with
        | None -> model, Cmd.none
        | Some gid ->
            let members   = sortedMembers model.GroupState
            let form      = model.SettlementForm
            let groupCur  = model.GroupState.Currency
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
                    let actorId = findActorId model.GroupState model.Auth
                    let date    = parseFormDate form.DateText
                    let notes   = if form.Notes.Trim() = "" then None else Some (form.Notes.Trim())
                    let save1 = Commands.recordSettlement gid actorId fromId toId amount form.Currency rate date notes
                    let save2 =
                        match form.UseSecondPayment, amt2 with
                        | true, Some a2 when a2 > 0m ->
                            Commands.recordSettlement gid actorId fromId toId a2 form.Currency2 rate2 date notes
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
        match model.ActiveGroupId with
        | Some gid ->
            let cmd = Cmd.OfAsync.perform Storage.loadGroupState gid (fun gs -> GroupLoaded (gid, gs))
            { model with SettlementForm = { model.SettlementForm with IsSubmitting = false } }, cmd
        | None ->
            { model with Page = GroupOverview }, Cmd.none

    | SettlementSaved (Error err) ->
        { model with SettlementForm = { model.SettlementForm with IsSubmitting = false; Error = Some err } }, Cmd.none

    | GroupStateUpdated gs ->
        { model with GroupState = gs }, Cmd.none

    | RemoteEventReceived gid ->
        let cmd = Cmd.OfAsync.perform Storage.loadGroupState gid (fun gs -> GroupStateUpdated gs)
        model, cmd

    | SyncDone ->
        model, Cmd.none

let subscribe (model: Model) : Sub<Msg> =
    let authSub (dispatch: Msg -> unit) =
        let unsub = Auth.subscribe (AuthReceived >> dispatch)
        { new System.IDisposable with member _.Dispose() = unsub () }

    let baseSubs = [ ["auth"], authSub ]

    match model.ActiveGroupId with
    | None -> baseSubs
    | Some gid ->
        let (GroupId g) = gid
        let realtimeSub (dispatch: Msg -> unit) =
            let unsub = Sync.subscribeGroup gid (fun () -> dispatch (RemoteEventReceived gid))
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

let private loadingView () =
    Html.div [
        prop.className "min-h-screen bg-gray-50 flex items-center justify-center"
        prop.children [
            Html.p [ prop.className "text-gray-500 text-sm"; prop.text "Loading..." ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    match model.Page with
    | Loading             -> loadingView ()
    | SignIn              -> signInView model dispatch
    | GroupOverview       -> GroupPage.view model.GroupState dispatch
    | AddExpense          -> ExpenseFormPage.view model.GroupState model.ExchangeRates model.ExpenseForm dispatch
    | RecordSettlement    -> SettlementFormPage.view model.GroupState model.ExchangeRates model.SettlementForm dispatch
    | Analytics           -> loadingView ()  // placeholder until analytics page is built
    | CreateContext       -> ContextFormPage.view model.GroupState model.ContextForm dispatch
    | ContextDetail ctxId -> ContextDetailPage.view model.GroupState ctxId dispatch
