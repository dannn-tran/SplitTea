module UITypes

open SplitTea.Core

type Page =
    | SignIn
    | Loading
#if DEVMODE
    | DevBootstrap
#endif
    | SpaceOverview
    | AddExpense
    | RecordSettlement
    | Analytics

type ExpenseForm = {
    Description      : string
    AmountText       : string
    Currency         : string
    ExchangeRateText : string
    PaidByIndex      : int
    DateText         : string
    Category         : string
    Notes            : string
    IsSubmitting     : bool
    Error            : string option
    IsAddingCategory : bool
    NewCategoryText  : string
}

type SettlementForm = {
    FromIndex         : int
    ToIndex           : int
    AmountText        : string
    Currency          : string
    ExchangeRateText  : string
    UseSecondPayment  : bool
    AmountText2       : string
    Currency2         : string
    ExchangeRateText2 : string
    DateText          : string
    Notes             : string
    IsSubmitting      : bool
    Error             : string option
}

type Model = {
    Auth             : Auth.AuthUser option
    Page             : Page
    ActiveSpaceId    : SpaceId option
    SpaceState       : SpaceState
    CategoryFilter   : string
    NewCategory      : string
    EditingCategory  : string option
    EditCategoryName : string
    CategoryError    : string option
    EditingExpenseId : ExpenseId option
    ExchangeRates    : Map<string, decimal>
    SignInEmail      : string
    SignInError      : string option
    IsAuthLoading    : bool
    ExpenseForm      : ExpenseForm
    SettlementForm   : SettlementForm
    Toast            : string option
    ShowSettings     : bool
    ShowAnalytics    : bool
#if DEVMODE
    DevActorId       : MemberId option
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
    | SpaceLoaded       of SpaceId * SpaceState
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
    | AnalyticsToggled
#if DEVMODE
    | DevActorSet              of MemberId
    | DevReset
#endif
