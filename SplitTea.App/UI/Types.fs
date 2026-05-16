module UITypes

open SplitTea.Core

type Page =
    | SignIn
    | Loading
#if DEVMODE
    | DevBootstrap
#endif
    | GroupOverview
    | AddExpense
    | RecordSettlement
    | Analytics
    | CreateContext
    | ContextDetail of SplitTea.Core.ContextId

type ContextForm = {
    Name         : string
    Template     : SplitTea.Core.ContextTemplate
    DateFromText : string
    DateToText   : string
    IsSubmitting : bool
    Error        : string option
}

type ExpenseForm = {
    Description      : string
    AmountText       : string
    Currency         : string
    ExchangeRateText : string
    PaidByIndex      : int
    DateText         : string
    Category         : string
    Notes            : string
    ContextIndex     : int  // 0 = no context, 1+ = index into sorted context list
    IsSubmitting     : bool
    Error            : string option
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
    Auth           : Auth.AuthUser option
    Page           : Page
    ActiveGroupId  : GroupId option
    GroupState     : GroupState
    ExchangeRates  : Map<string, decimal>  // base = group currency → other currencies
    SignInEmail    : string
    SignInError    : string option
    IsAuthLoading  : bool
    ContextForm    : ContextForm
    ExpenseForm    : ExpenseForm
    SettlementForm : SettlementForm
}

type Msg =
    | AuthReceived      of Auth.AuthEvent
#if DEVMODE
    | CreateDevGroup
    | DevGroupCreated   of Result<GroupId, string>
#endif
    | SignInEmailSet     of string
    | SignInSubmit
    | SignInDone        of Result<unit, string>
    | SignOut
    | GroupLoaded       of GroupId * GroupState
    | GroupNotFound
    | NavigateTo        of Page
    | AddExpenseClick
    | RecordSettlementClick
    | ExpenseFormSet    of ExpenseForm
    | ExpenseSubmit
    | ExpenseSaved      of Result<unit, string>
    | SettlementFormSet    of SettlementForm
    | SettlementSubmit
    | SettlementSaved      of Result<unit, string>
    | GroupStateUpdated    of GroupState
    | RemoteEventReceived  of GroupId
    | SyncDone
    | ExchangeRatesLoaded  of Map<string, decimal>
    | CreateContextClick
    | ContextFormSet       of ContextForm
    | ContextSubmit
    | ContextSaved         of Result<unit, string>
    | OpenContext          of ContextId
