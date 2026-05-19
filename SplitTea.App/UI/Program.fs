module AppRoot

open Elmish
open Feliz
open Fable.Core
open Fable.Core.JsInterop
open SplitTea.Core
open UITypes
open AppHelpers

let private deferredInstallPrompt : obj option ref = ref None

let init () : Model * Cmd<Msg> =
    let activeSpaceId = getActiveSpaceId ()
    let model = {
        Auth             = None
        Page             = Loading
        ActiveSpaceId    = activeSpaceId
        SpaceState       = SpaceState.Empty
        CategoryFilter   = ""
        NewCategory      = ""
        EditingCategory  = None
        EditCategoryName = ""
        CategoryError    = None
        EditingExpenseId = None
        ExchangeRates    = Map.empty
        SignInEmail      = ""
        SignInError      = None
        IsAuthLoading    = true
        ExpenseForm      = emptyExpenseForm "" []
        SettlementForm   = emptySettlementForm [] ""
        Toast                  = None
        ShowSettings           = false
        IsEditingSpaceName     = false
        SpaceNameText          = ""
        IsEditingProfileName   = false
        ProfileNameText        = ""
        ProfileNameError       = None
        ShowSpaceSwitcher      = false
        Conflicts              = []
        KnownSpaces            = Storage.getKnownSpaces ()
        CreateSpaceForm        = emptyCreateSpaceForm
        ConfirmDialog          = None
        InstallPromptAvailable = false
#if DEVMODE
        DevActorId = getDevActorId ()
#endif
    }
    let cmd =
        match activeSpaceId with
        | Some sid ->
            let seedCmd =
                if not (Storage.getKnownSpaces () |> List.exists (fun s -> s.Id = sid)) then
                    Cmd.OfFunc.attempt (fun () -> Storage.upsertKnownSpace sid "") () (fun _ -> SyncDone)
                else Cmd.none
            let loadCmd = Cmd.OfAsync.perform Storage.rebaseAndDisplay sid (fun (state, conflicts) -> SpaceLoaded (sid, state, conflicts))
            Cmd.batch [ seedCmd; loadCmd ]
        | None -> Cmd.none
    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | AuthReceived e               -> AuthUpdate.handle e model
    | SignInEmailSet email         -> AuthUpdate.handleSignInEmailSet email model
    | SignInSubmit                 -> AuthUpdate.handleSignInSubmit model
    | SignInDone result            -> AuthUpdate.handleSignInDone result model
    | SignOut                      -> AuthUpdate.handleSignOut model
    | SpaceLoaded (sid, s, cs)     -> SpaceUpdate.handleSpaceLoaded sid s cs model
    | SpaceNotFound                -> SpaceUpdate.handleSpaceNotFound model
    | SpaceRebased (s, cs)         -> SpaceUpdate.handleSpaceRebased s cs model
    | SwitchToSpace sid            -> SpaceUpdate.handleSwitchToSpace sid model
    | StartSpaceRename             -> SpaceUpdate.handleStartSpaceRename model
    | SpaceNameTextSet text        -> SpaceUpdate.handleSpaceNameTextSet text model
    | SaveSpaceRename              -> SpaceUpdate.handleSaveSpaceRename model
    | SpaceNameSaved result        -> SpaceUpdate.handleSpaceNameSaved result model
    | DeleteSpaceClick             -> SpaceUpdate.handleDeleteSpaceClick model
    | SpaceDeleted result          -> SpaceUpdate.handleSpaceDeleted result model
    | CategoryFilterSet cat        -> CategoryUpdate.handleCategoryFilterSet cat model
    | NewCategorySet name          -> CategoryUpdate.handleNewCategorySet name model
    | StartCategoryRename name     -> CategoryUpdate.handleStartCategoryRename name model
    | EditCategoryNameSet name     -> CategoryUpdate.handleEditCategoryNameSet name model
    | AddCategorySubmit            -> CategoryUpdate.handleAddCategorySubmit model
    | SaveCategoryRename           -> CategoryUpdate.handleSaveCategoryRename model
    | ArchiveCategory name         -> CategoryUpdate.handleArchiveCategory name model
    | CategorySaved result         -> CategoryUpdate.handleCategorySaved result model
    | AddCategoryFromForm          -> CategoryUpdate.handleAddCategoryFromForm model
    | CategoryFromFormSaved result -> CategoryUpdate.handleCategoryFromFormSaved result model
    | AddExpenseClick              -> ExpenseUpdate.handleAddExpenseClick model
    | EditExpenseClick id          -> ExpenseUpdate.handleEditExpenseClick id model
    | DeleteExpenseClick id        -> ExpenseUpdate.handleDeleteExpenseClick id model
    | ExpenseFormSet form          -> ExpenseUpdate.handleExpenseFormSet form model
    | ExpenseSubmit                -> ExpenseUpdate.handleExpenseSubmit model
    | ExpenseSaved result          -> ExpenseUpdate.handleExpenseSaved result model
    | ExpenseCorrected result      -> ExpenseUpdate.handleExpenseCorrected result model
    | ExpenseDeleted result        -> ExpenseUpdate.handleExpenseDeleted result model
    | RecordSettlementClick        -> SettlementUpdate.handleRecordSettlementClick model
    | SettlementFromSuggestion s   -> SettlementUpdate.handleSettlementFromSuggestion s model
    | SettlementFormSet form       -> SettlementUpdate.handleSettlementFormSet form model
    | SettlementSubmit             -> SettlementUpdate.handleSettlementSubmit model
    | SettlementSaved result       -> SettlementUpdate.handleSettlementSaved result model
    | StartProfileRename           -> ProfileUpdate.handleStartProfileRename model
    | ProfileNameTextSet text      -> ProfileUpdate.handleProfileNameTextSet text model
    | SaveProfileRename            -> ProfileUpdate.handleSaveProfileRename model
    | ProfileNameSaved result      -> ProfileUpdate.handleProfileNameSaved result model
    | SpaceStateUpdated ss         -> SyncUpdate.handleSpaceStateUpdated ss model
    | RemoteEventReceived sid      -> SyncUpdate.handleRemoteEventReceived sid model
    | SyncOutcome (sid, outcomes)  -> SyncUpdate.handleSyncOutcome sid outcomes model
    | FlushPending                 -> SyncUpdate.handleFlushPending model
    | DismissConflict eventId      -> SyncUpdate.handleDismissConflict eventId model
    | ExchangeRatesLoaded rates    -> SyncUpdate.handleExchangeRatesLoaded rates model
    | ToastCleared                 -> SyncUpdate.handleToastCleared model
    | SyncDone                     -> model, Cmd.none
    | NavigateTo page              -> { model with Page = page }, Cmd.none
    | SettingsToggled              -> SettingsUpdate.handleSettingsToggled model
    | SpaceSwitcherToggled         -> SettingsUpdate.handleSpaceSwitcherToggled model
    | RequestConfirm req           -> SettingsUpdate.handleRequestConfirm req model
    | ConfirmResolved confirmed    -> SettingsUpdate.handleConfirmResolved confirmed model
    | InstallPromptReady           -> SettingsUpdate.handleInstallPromptReady model
    | InstallApp                   -> SettingsUpdate.handleInstallApp deferredInstallPrompt model
    | DismissInstallPrompt         -> SettingsUpdate.handleDismissInstallPrompt deferredInstallPrompt model
    | CreateSpaceNameSet text      -> CreateSpaceUpdate.handleCreateSpaceNameSet text model
    | CreateSpaceCurrencySet text  -> CreateSpaceUpdate.handleCreateSpaceCurrencySet text model
    | CreateSpaceMemberSet text    -> CreateSpaceUpdate.handleCreateSpaceMemberSet text model
    | CreateSpaceSubmit            -> CreateSpaceUpdate.handleCreateSpaceSubmit model
    | CreateSpaceDone result       -> CreateSpaceUpdate.handleCreateSpaceDone result model
#if DEVMODE
    | CreateDevSpace ->
        let cmd =
            Cmd.OfAsync.either
                DevBootstrap.createLocalSpace
                ()
                (fun sid -> DevSpaceCreated (Ok sid))
                (fun ex  -> DevSpaceCreated (Error ex.Message))
        { model with IsAuthLoading = true }, cmd
    | DevSpaceCreated (Ok sid) ->
        let cmd = Cmd.OfAsync.perform Storage.rebaseAndDisplay sid (fun (state, conflicts) -> SpaceLoaded (sid, state, conflicts))
        { model with IsAuthLoading = false }, cmd
    | DevSpaceCreated (Error _) ->
        { model with IsAuthLoading = false }, Cmd.none
    | DevActorSet memberId ->
        setDevActorId memberId
        { model with DevActorId = Some memberId }, Cmd.none
    | DevReset ->
        let sessionStore : obj = emitJsExpr () "sessionStorage"
        let browserStore : obj = emitJsExpr () "localStorage"
        let cmd =
            Cmd.OfAsync.attempt
                (fun () -> async {
                    do! IndexedDb.clearAllEvents ()
                    browserStore?removeItem("activeSpaceId") |> ignore
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

    let installPromptSub (dispatch: Msg -> unit) =
        let handler : obj =
            System.Func<obj, unit>(fun e ->
                e?preventDefault() |> ignore
                deferredInstallPrompt.Value <- Some e
                dispatch InstallPromptReady) |> box
        emitJsExpr (handler) "window.addEventListener('beforeinstallprompt', $0)" |> ignore
        { new System.IDisposable with
            member _.Dispose() = emitJsExpr (handler) "window.removeEventListener('beforeinstallprompt', $0)" |> ignore }

    let baseSubs = [ ["auth"], authSub; ["online"], onlineSub; ["install-prompt"], installPromptSub ]

    match model.ActiveSpaceId with
    | None -> baseSubs
    | Some sid ->
        let (SpaceId g) = sid
        if DevMode.isEnabled () then
            let devBroadcastSub (dispatch: Msg -> unit) =
                let channel : obj = emitJsExpr () "new BroadcastChannel('splittea-dev')"
                channel?addEventListener("message", fun (e: obj) ->
                    try dispatch (RemoteEventReceived (SpaceId (System.Guid.Parse (string e?data))))
                    with _ -> ()) |> ignore
                { new System.IDisposable with member _.Dispose() = channel?close() |> ignore }
            baseSubs @ [ ["dev-broadcast"; string g], devBroadcastSub ]
        else
            let realtimeSub (dispatch: Msg -> unit) =
                let unsub = Sync.subscribeSpace sid (fun () -> dispatch (RemoteEventReceived sid))
                { new System.IDisposable with member _.Dispose() = unsub () }
            baseSubs @ [ ["realtime"; string g], realtimeSub ]

let private loadingView () =
    Html.div [
        prop.className "min-h-screen bg-gray-50 flex items-center justify-center"
        prop.children [
            Html.p [ prop.className "text-gray-500 text-sm"; prop.text "Loading..." ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    let isSpaceTab = model.Page = SpaceOverview || model.Page = Analytics || model.Page = Activity || model.Page = Profile

    let currentMember =
        if isSpaceTab then
            let actorId = resolveActor model
            model.SpaceState.Members |> Map.tryFind actorId
        else None

    let skipLink =
        Html.a [
            prop.href "#main-content"
            prop.className "sr-only focus:not-sr-only focus:fixed focus:top-2 focus:left-2 focus:z-[100] focus:px-4 focus:py-2 focus:bg-teal-600 focus:text-white focus:rounded-lg focus:text-sm focus:font-medium"
            prop.text "Skip to main content"
        ]

    let pageContent =
        match model.Page with
        | Loading          -> loadingView ()
        | SignIn           -> SignInPage.view model dispatch
#if DEVMODE
        | DevBootstrap     -> SignInPage.devBootstrapView model dispatch
#endif
        | SpaceOverview    -> SpacePage.view model.SpaceState model dispatch
        | Analytics        -> AnalyticsPage.view model.SpaceState model.ExchangeRates
        | Activity         -> ActivityPage.view model.SpaceState
        | Profile          ->
            let displayName = currentMember |> Option.map _.DisplayName |> Option.defaultValue ""
            let email       = model.Auth |> Option.bind _.Email
            ProfilePage.view displayName email model dispatch
        | AddExpense       -> ExpenseFormPage.view model.SpaceState model.ExchangeRates model.ExpenseForm model.EditingExpenseId.IsSome dispatch
        | RecordSettlement -> SettlementFormPage.view model.SpaceState model.ExchangeRates model.SettlementForm dispatch
        | CreateSpace      -> CreateSpacePage.view model.ActiveSpaceId.IsSome model dispatch

    let inner =
        Html.main [
            prop.id "main-content"
            prop.tabIndex -1
            prop.className "outline-none"
            prop.children [ pageContent ]
        ]

    let toast =
        Html.div [
            prop.role "status"
            prop.ariaLive.polite
            prop.className "fixed top-4 inset-x-0 flex justify-center z-50 pointer-events-none"
            prop.children [
                match model.Toast with
                | Some msg ->
                    Html.div [
                        prop.className "bg-gray-800 text-white text-sm font-medium px-4 py-2.5 rounded-full shadow-lg"
                        prop.text msg
                    ]
                | None -> Html.none
            ]
        ]

    let nav            = if isSpaceTab then BottomNav.bottomNav model dispatch else Html.none
    let switcher       = if model.ShowSpaceSwitcher then BottomNav.spaceSwitcherSheet model dispatch else Html.none
    let conflictBanner = Overlays.conflictBanner model dispatch isSpaceTab
    let confirmModal   = Overlays.confirmModal model dispatch

#if DEVMODE
    if DevMode.isEnabled () && not model.SpaceState.Members.IsEmpty then
        Html.div [ prop.children [ skipLink; inner; nav; Overlays.devActorBadge model dispatch; conflictBanner; switcher; toast; confirmModal ] ]
    else
        Html.div [ prop.children [ skipLink; inner; nav; conflictBanner; switcher; toast; confirmModal ] ]
#else
    Html.div [ prop.children [ skipLink; inner; nav; conflictBanner; switcher; toast; confirmModal ] ]
#endif
