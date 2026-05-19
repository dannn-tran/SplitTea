module UITypes

open SplitTea.Core

type Field<'T> = {
    Text    : string
    Parsed  : 'T option
    IsError : bool
}

[<RequireQualifiedAccess>]
module Field =
    let emptyDecimal : Field<decimal> = { Text = ""; Parsed = None; IsError = false }

    let ofAmount (v: decimal) : Field<decimal> =
        { Text = sprintf "%.2f" v; Parsed = Some v; IsError = false }

    let ofRate (v: decimal) : Field<decimal> =
        { Text = string v; Parsed = Some v; IsError = false }

    let parseDecimal (text: string) : Field<decimal> =
        let t = text.Trim()
        if t = "" then { Text = text; Parsed = None; IsError = false }
        else
            try { Text = text; Parsed = Some (decimal t); IsError = false }
            with _ -> { Text = text; Parsed = None; IsError = true }

type Page =
    | SignIn
    | Loading
#if DEVMODE
    | DevBootstrap
#endif
    | SpaceOverview
    | Analytics
    | Activity
    | Profile
    | AddExpense
    | RecordSettlement
    | CreateSpace

type CreateSpaceForm = {
    SpaceNameText : string
    CurrencyText  : string
    MemberName    : string
    IsSubmitting  : bool
    Error         : string option
}

type ConfirmAction =
    | ConfirmDeleteExpense of ExpenseId
    | ConfirmLeaveSpace

type ConfirmRequest = {
    Message : string
    Action  : ConfirmAction
}

type SplitMode = EqualSplit | CustomSplit

type ExpenseForm = {
    Description  : string
    Amount       : Field<decimal>
    Currency     : string
    ExchangeRate : Field<decimal>
    PaidById     : MemberId
    DateText         : string
    Category         : string
    Notes            : string
    IsSubmitting     : bool
    Error            : string option
    IsAddingCategory : bool
    NewCategoryText  : string
    SplitMode        : SplitMode
    Included         : Set<MemberId>
    CustomAmounts    : Map<MemberId, string>
}

type SettlementForm = {
    FromId           : MemberId
    ToId             : MemberId
    Amount           : Field<decimal>
    Currency         : string
    ExchangeRate     : Field<decimal>
    UseSecondPayment : bool
    Amount2          : Field<decimal>
    Currency2        : string
    ExchangeRate2    : Field<decimal>
    DateText         : string
    Notes            : string
    IsSubmitting     : bool
    Error            : string option
}

type Model = {
    Auth                 : Auth.AuthUser option
    Page                 : Page
    ActiveSpaceId        : SpaceId option
    SpaceState           : SpaceState
    Conflicts            : Storage.ConflictedEvent list
    CategoryFilter       : string
    NewCategory          : string
    EditingCategory      : string option
    EditCategoryName     : string
    CategoryError        : string option
    EditingExpenseId     : ExpenseId option
    ExchangeRates        : Map<string, decimal>
    SignInEmail          : string
    SignInError          : string option
    IsAuthLoading        : bool
    ExpenseForm          : ExpenseForm
    SettlementForm       : SettlementForm
    Toast                : string option
    ShowSettings         : bool
    IsEditingSpaceName   : bool
    SpaceNameText        : string
    IsEditingProfileName : bool
    ProfileNameText      : string
    ProfileNameError     : string option
    ShowSpaceSwitcher    : bool
    KnownSpaces          : Storage.SpaceSummary list
    CreateSpaceForm      : CreateSpaceForm
    ConfirmDialog            : ConfirmRequest option
    InstallPromptAvailable   : bool
#if DEVMODE
    DevActorId               : MemberId option
#endif
}

type Msg =
    | AuthReceived      of Auth.AuthEvent
#if DEVMODE
    | CreateDevSpace
    | DevSpaceCreated   of Result<SpaceId, string>
#endif
    | SignInEmailSet     of string
    | SignInSubmit
    | SignInDone        of Result<unit, string>
    | SignOut
    | SpaceLoaded       of SpaceId * SpaceState * Storage.ConflictedEvent list
    | SpaceNotFound
    | NavigateTo        of Page
    | AddExpenseClick
    | EditExpenseClick    of ExpenseId
    | DeleteExpenseClick  of ExpenseId
    | RecordSettlementClick
    | ExpenseFormSet    of ExpenseForm
    | ExpenseSubmit
    | ExpenseSaved      of Result<unit, string>
    | ExpenseCorrected  of Result<unit, string>
    | ExpenseDeleted    of Result<unit, string>
    | SettlementFormSet    of SettlementForm
    | SettlementSubmit
    | SettlementSaved      of Result<unit, string>
    | SpaceStateUpdated    of SpaceState
    | RemoteEventReceived  of SpaceId
    | SpaceRebased         of SpaceState * Storage.ConflictedEvent list
    | SyncOutcome          of SpaceId * Sync.PushOutcome list
    | FlushPending
    | DismissConflict      of EventId
    | CategoryFilterSet    of string
    | NewCategorySet       of string
    | AddCategorySubmit
    | StartCategoryRename  of string
    | EditCategoryNameSet  of string
    | SaveCategoryRename
    | ArchiveCategory      of string
    | CategorySaved            of Result<unit, string>
    | SettlementFromSuggestion of SuggestedSettlement
    | AddCategoryFromForm
    | CategoryFromFormSaved    of Result<string, string>
    | SyncDone
    | ExchangeRatesLoaded      of Map<string, decimal>
    | ToastCleared
    | SettingsToggled
    | StartSpaceRename
    | SpaceNameTextSet   of string
    | SaveSpaceRename
    | SpaceNameSaved     of Result<unit, string>
    | StartProfileRename
    | ProfileNameTextSet of string
    | SaveProfileRename
    | ProfileNameSaved   of Result<unit, string>
    | SpaceSwitcherToggled
    | SwitchToSpace          of SpaceId
    | DeleteSpaceClick
    | SpaceDeleted           of Result<unit, string>
    | RequestConfirm         of ConfirmRequest
    | ConfirmResolved        of bool
    | InstallPromptReady
    | InstallApp
    | DismissInstallPrompt
    | CreateSpaceNameSet     of string
    | CreateSpaceCurrencySet of string
    | CreateSpaceMemberSet   of string
    | CreateSpaceSubmit
    | CreateSpaceDone        of Result<SpaceId, string>
#if DEVMODE
    | DevActorSet              of MemberId
    | DevReset
#endif
