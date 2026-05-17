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

let private emptyExpenseForm (groupCurrency: string) : ExpenseForm = {
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
        CategoryError  = None
        ExchangeRates  = Map.empty
        SignInEmail    = ""
        SignInError    = None
        IsAuthLoading  = true
        ExpenseForm    = emptyExpenseForm ""
        SettlementForm = emptySettlementForm 0 ""
#if DEVMODE
        DevActorId     = getDevActorId ()
#endif
    }
    let cmd =
        match activeSpaceId with
        | Some sid -> Cmd.OfAsync.perform Storage.loadSpaceState sid (fun ss -> SpaceLoaded (sid, ss))
        | None     -> Cmd.none
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
                else SignIn
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
        let syncCmd = Cmd.OfAsync.attempt (fun () -> Sync.pushPending ()) () (fun _ -> SyncDone)
        let fxCmd   =
            Cmd.OfAsync.either
                (fun () -> FxRates.getRates ss.Currency)
                ()
                ExchangeRatesLoaded
                (fun _ -> ExchangeRatesLoaded Map.empty)
        { model with ActiveSpaceId = Some sid; SpaceState = ss; Page = SpaceOverview }, Cmd.batch [ syncCmd; fxCmd ]

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
        { model with Page = AddExpense; ExpenseForm = emptyExpenseForm cur }, Cmd.none

    | RecordSettlementClick ->
        let n   = model.SpaceState.Members.Count
        let cur = model.SpaceState.Currency
        { model with Page = RecordSettlement; SettlementForm = emptySettlementForm n cur }, Cmd.none

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
                    let actorId  = resolveActor model
                    let split    = Equal (members |> List.map (fun m -> m.Id))
                    let date     = parseFormDate form.DateText
                    let category = if form.Category = "" then None else Some form.Category
                    let notes    = if form.Notes.Trim() = "" then None else Some (form.Notes.Trim())
                    let cmd =
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
            let cmd = Cmd.OfAsync.perform Storage.loadSpaceState sid (fun ss -> SpaceLoaded (sid, ss))
            { model with ExpenseForm = { model.ExpenseForm with IsSubmitting = false } }, cmd
        | None ->
            { model with Page = SpaceOverview }, Cmd.none

    | ExpenseSaved (Error err) ->
        { model with ExpenseForm = { model.ExpenseForm with IsSubmitting = false; Error = Some err } }, Cmd.none

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
            let cmd = Cmd.OfAsync.perform Storage.loadSpaceState sid (fun ss -> SpaceLoaded (sid, ss))
            { model with SettlementForm = { model.SettlementForm with IsSubmitting = false } }, cmd
        | None ->
            { model with Page = SpaceOverview }, Cmd.none

    | SettlementSaved (Error err) ->
        { model with SettlementForm = { model.SettlementForm with IsSubmitting = false; Error = Some err } }, Cmd.none

    | SpaceStateUpdated ss ->
        { model with SpaceState = ss }, Cmd.none

    | RemoteEventReceived sid ->
        let cmd = Cmd.OfAsync.perform Storage.loadSpaceState sid (fun ss -> SpaceStateUpdated ss)
        model, cmd

    | SyncDone ->
        model, Cmd.none

#if DEVMODE
    | DevActorSet memberId ->
        setDevActorId memberId
        { model with DevActorId = Some memberId }, Cmd.none
#endif

let subscribe (model: Model) : Sub<Msg> =
    let authSub (dispatch: Msg -> unit) =
        let unsub = Auth.subscribe (AuthReceived >> dispatch)
        { new System.IDisposable with member _.Dispose() = unsub () }

    let baseSubs = [ ["auth"], authSub ]

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
        prop.className "fixed bottom-4 right-4 z-50 bg-gray-800 text-white text-xs rounded-lg px-3 py-2 shadow-lg flex items-center gap-2"
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
        ]
    ]
#endif

let view (model: Model) (dispatch: Msg -> unit) =
    let inner =
        match model.Page with
        | Loading             -> loadingView ()
        | SignIn              -> signInView model dispatch
#if DEVMODE
        | DevBootstrap        -> devBootstrapView model dispatch
#endif
        | SpaceOverview       -> SpacePage.view model.SpaceState model dispatch
        | AddExpense          -> ExpenseFormPage.view model.SpaceState model.ExchangeRates model.ExpenseForm dispatch
        | RecordSettlement    -> SettlementFormPage.view model.SpaceState model.ExchangeRates model.SettlementForm dispatch
        | Analytics           -> loadingView ()  // placeholder until analytics page is built
#if DEVMODE
    if DevMode.isEnabled () && not model.SpaceState.Members.IsEmpty then
        Html.div [
            prop.children [ inner; devActorBadge model dispatch ]
        ]
    else inner
#else
    inner
#endif
