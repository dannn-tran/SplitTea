module CreateSpaceUpdate

open Elmish
open SplitTea.Core
open UITypes
open AppHelpers

let handleCreateSpaceNameSet (text: string) (model: Model) : Model * Cmd<Msg> =
    { model with CreateSpaceForm = { model.CreateSpaceForm with SpaceNameText = text; Error = None } }, Cmd.none

let handleCreateSpaceCurrencySet (text: string) (model: Model) : Model * Cmd<Msg> =
    { model with CreateSpaceForm = { model.CreateSpaceForm with CurrencyText = text; Error = None } }, Cmd.none

let handleCreateSpaceMemberSet (text: string) (model: Model) : Model * Cmd<Msg> =
    { model with CreateSpaceForm = { model.CreateSpaceForm with MemberName = text; Error = None } }, Cmd.none

let handleCreateSpaceSubmit (model: Model) : Model * Cmd<Msg> =
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

let handleCreateSpaceDone (result: Result<SpaceId, string>) (model: Model) : Model * Cmd<Msg> =
    match result with
    | Ok sid ->
        let name = model.CreateSpaceForm.SpaceNameText.Trim()
        Storage.upsertKnownSpace sid name
        setActiveSpaceId sid
        let cmd = Cmd.OfAsync.perform Storage.rebaseAndDisplay sid (fun (state, conflicts) -> SpaceLoaded (sid, state, conflicts))
        { model with CreateSpaceForm = emptyCreateSpaceForm; ActiveSpaceId = Some sid }, cmd
    | Error err ->
        { model with CreateSpaceForm = { model.CreateSpaceForm with IsSubmitting = false; Error = Some err } }, Cmd.none
