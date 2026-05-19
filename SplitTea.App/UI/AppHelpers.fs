module AppHelpers

open Fable.Core
open Fable.Core.JsInterop
open SplitTea.Core
open UITypes

let private browserStorage : obj = emitJsExpr () "localStorage"

#if DEVMODE
let private sessionStore : obj = emitJsExpr () "sessionStorage"

let getDevActorId () : MemberId option =
    let v : obj = sessionStore?getItem("devActorId")
    if isNull v then None
    else try Some (MemberId (System.Guid.Parse (string v))) with _ -> None

let setDevActorId (id: MemberId) =
    let (MemberId g) = id
    sessionStore?setItem("devActorId", string g) |> ignore
#endif

let getActiveSpaceId () : SpaceId option =
    let v : obj = browserStorage?getItem("activeSpaceId")
    if isNull v then None
    else
        try Some (SpaceId (System.Guid.Parse (string v)))
        with _ -> None

let setActiveSpaceId (id: SpaceId) =
    let (SpaceId g) = id
    browserStorage?setItem("activeSpaceId", string g) |> ignore

let clearActiveSpaceId () =
    browserStorage?removeItem("activeSpaceId") |> ignore

let todayStr () =
    let d = System.DateTime.Now
    sprintf "%04d-%02d-%02d" d.Year d.Month d.Day

let parseFormDate (s: string) =
    if s = "" then
        let d = System.DateTime.Now
        System.DateOnly(d.Year, d.Month, d.Day)
    else
        let parts = s.Split('-')
        System.DateOnly(int parts.[0], int parts.[1], int parts.[2])

let sortedMembers (state: SpaceState) =
    state.Members
    |> Map.toList
    |> List.map snd
    |> List.sortBy (fun m -> m.DisplayName)

let emptyCreateSpaceForm : CreateSpaceForm = {
    SpaceNameText = ""
    CurrencyText  = ""
    MemberName    = ""
    IsSubmitting  = false
    Error         = None
}

let emptyExpenseForm (groupCurrency: string) (members: Member list) : ExpenseForm =
    let defaultId = members |> List.tryHead |> Option.map (fun m -> m.Id) |> Option.defaultValue (MemberId System.Guid.Empty)
    {
        Description  = ""
        Amount       = Field.emptyDecimal
        Currency     = groupCurrency
        ExchangeRate = Field.emptyDecimal
        PaidById     = defaultId
        DateText     = todayStr ()
        Category     = ""
        Notes        = ""
        IsSubmitting = false
        Error        = None
        IsAddingCategory = false
        NewCategoryText  = ""
        SplitMode        = EqualSplit
        Included         = members |> List.map (fun m -> m.Id) |> Set.ofList
        CustomAmounts    = Map.empty
    }

let emptySettlementForm (members: Member list) (groupCurrency: string) : SettlementForm =
    let defId = members |> List.tryHead |> Option.map (fun m -> m.Id) |> Option.defaultValue (MemberId System.Guid.Empty)
    let toId  = members |> List.tryItem 1 |> Option.map (fun m -> m.Id) |> Option.defaultValue defId
    {
        FromId           = defId
        ToId             = toId
        Amount           = Field.emptyDecimal
        Currency         = groupCurrency
        ExchangeRate     = Field.emptyDecimal
        UseSecondPayment = false
        Amount2          = Field.emptyDecimal
        Currency2        = groupCurrency
        ExchangeRate2    = Field.emptyDecimal
        DateText         = todayStr ()
        Notes            = ""
        IsSubmitting     = false
        Error            = None
    }

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

let resolveActor (model: Model) : MemberId =
    let first () = model.SpaceState.Members |> Map.toList |> List.head |> fst
#if DEVMODE
    if DevMode.isEnabled () then
        match model.DevActorId with
        | Some id when Map.containsKey id model.SpaceState.Members -> id
        | _ -> first ()
    else
#endif
    findActorId model.SpaceState model.Auth
