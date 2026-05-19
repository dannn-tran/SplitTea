module SettlementUpdate

open Elmish
open SplitTea.Core
open UITypes
open AppHelpers

let handleRecordSettlementClick (model: Model) : Model * Cmd<Msg> =
    let members = sortedMembers model.SpaceState
    let cur     = model.SpaceState.Currency
    { model with Page = RecordSettlement; SettlementForm = emptySettlementForm members cur }, Cmd.none

let handleSettlementFromSuggestion (s: SuggestedSettlement) (model: Model) : Model * Cmd<Msg> =
    let members = sortedMembers model.SpaceState
    let cur     = model.SpaceState.Currency
    let form    = { emptySettlementForm members cur with
                        FromId = s.From
                        ToId   = s.To
                        Amount = Field.ofAmount s.Amount }
    { model with Page = RecordSettlement; SettlementForm = form }, Cmd.none

let handleSettlementFormSet (form: SettlementForm) (model: Model) : Model * Cmd<Msg> =
    { model with SettlementForm = form }, Cmd.none

let handleSettlementSubmit (model: Model) : Model * Cmd<Msg> =
    match model.ActiveSpaceId with
    | None -> model, Cmd.none
    | Some sid ->
        let form   = model.SettlementForm
        let fromId = form.FromId
        let toId   = form.ToId
        let amt2Opt =
            if not form.UseSecondPayment then Ok None
            elif form.Amount2.IsError then Error "Invalid second amount."
            else Ok form.Amount2.Parsed
        match form.Amount.Parsed, amt2Opt with
        | _, Error err ->
            { model with SettlementForm = { form with Error = Some err } }, Cmd.none
        | None, _ ->
            { model with SettlementForm = { form with Error = Some "Invalid amount." } }, Cmd.none
        | Some amount, Ok amt2 ->
            if fromId = toId then
                { model with SettlementForm = { form with Error = Some "From and To must be different members." } }, Cmd.none
            elif amount <= 0m then
                { model with SettlementForm = { form with Error = Some "Amount must be positive." } }, Cmd.none
            else
                let actorId = resolveActor model
                let date    = parseFormDate form.DateText
                let notes   = if form.Notes.Trim() = "" then None else Some (form.Notes.Trim())
                let leg1    = { Amount = amount; Currency = form.Currency }
                let legs    =
                    match form.UseSecondPayment, amt2 with
                    | true, Some a2 when a2 > 0m ->
                        [ leg1; { Amount = a2; Currency = form.Currency2 } ]
                    | _ -> [ leg1 ]
                let cmd =
                    Cmd.OfAsync.either
                        (fun () -> Commands.recordSettlement sid actorId fromId toId legs date notes)
                        ()
                        (fun () -> SettlementSaved (Ok ()))
                        (fun ex  -> SettlementSaved (Error ex.Message))
                { model with SettlementForm = { form with IsSubmitting = true; Error = None } }, cmd

let handleSettlementSaved (result: Result<unit, string>) (model: Model) : Model * Cmd<Msg> =
    match result with
    | Ok () ->
        match model.ActiveSpaceId with
        | Some sid ->
            let loadCmd  = Cmd.OfAsync.perform Storage.rebaseAndDisplay sid SpaceRebased
            let clearCmd = Cmd.OfAsync.perform (fun () -> Async.Sleep Styles.toastDurationMs) () (fun () -> ToastCleared)
            { model with SettlementForm = { model.SettlementForm with IsSubmitting = false }; Toast = Some "Settlement recorded!" },
            Cmd.batch [ loadCmd; clearCmd ]
        | None ->
            { model with Page = SpaceOverview }, Cmd.none
    | Error err ->
        { model with SettlementForm = { model.SettlementForm with IsSubmitting = false; Error = Some err } }, Cmd.none
