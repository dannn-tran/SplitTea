module SpaceUpdate

open Elmish
open SplitTea.Core
open UITypes
open AppHelpers

let handleSpaceLoaded (sid: SpaceId) (state: SpaceState) (conflicts: Storage.ConflictedEvent list) (model: Model) : Model * Cmd<Msg> =
    setActiveSpaceId sid
    Storage.upsertKnownSpace sid state.Name
    let authToken = model.Auth |> Option.map (fun u -> u.AccessToken) |> Option.defaultValue ""
    let syncCmd   = Cmd.OfAsync.perform (fun () -> Sync.pushPending authToken) () (fun outcomes -> SyncOutcome (sid, outcomes))
    let fxCmd     =
        Cmd.OfAsync.either
            (fun () -> FxRates.getRates state.Currency)
            ()
            ExchangeRatesLoaded
            (fun _ -> ExchangeRatesLoaded Map.empty)
    { model with ActiveSpaceId = Some sid; SpaceState = state; Conflicts = conflicts; Page = SpaceOverview
                 KnownSpaces = Storage.getKnownSpaces () }, Cmd.batch [ syncCmd; fxCmd ]

let handleSpaceNotFound (model: Model) : Model * Cmd<Msg> =
    { model with Page = SignIn }, Cmd.none

let handleSpaceRebased (state: SpaceState) (newConflicts: Storage.ConflictedEvent list) (model: Model) : Model * Cmd<Msg> =
    let allConflicts = model.Conflicts @ newConflicts
    let toast =
        if newConflicts.IsEmpty then model.Toast
        else
            let summary = newConflicts |> List.map (fun c -> sprintf "%s: %s" c.Description c.Reason) |> String.concat "\n"
            Some (sprintf "Some changes could not be applied:\n%s" summary)
    { model with SpaceState = state; Conflicts = allConflicts; Toast = toast }, Cmd.none

let handleSwitchToSpace (sid: SpaceId) (model: Model) : Model * Cmd<Msg> =
    if model.ActiveSpaceId = Some sid then
        { model with ShowSpaceSwitcher = false }, Cmd.none
    else
        setActiveSpaceId sid
        let cmd = Cmd.OfAsync.perform Storage.rebaseAndDisplay sid (fun (state, conflicts) -> SpaceLoaded (sid, state, conflicts))
        { model with ShowSpaceSwitcher = false; ActiveSpaceId = Some sid; Page = SpaceOverview }, cmd

let handleStartSpaceRename (model: Model) : Model * Cmd<Msg> =
    { model with IsEditingSpaceName = true; SpaceNameText = model.SpaceState.Name }, Cmd.none

let handleSpaceNameTextSet (text: string) (model: Model) : Model * Cmd<Msg> =
    { model with SpaceNameText = text }, Cmd.none

let handleSaveSpaceRename (model: Model) : Model * Cmd<Msg> =
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

let handleSpaceNameSaved (result: Result<unit, string>) (model: Model) : Model * Cmd<Msg> =
    match result with
    | Ok () ->
        match model.ActiveSpaceId with
        | Some sid ->
            let cmd = Cmd.OfAsync.perform Storage.rebaseAndDisplay sid SpaceRebased
            { model with IsEditingSpaceName = false; SpaceNameText = "" }, cmd
        | None -> { model with IsEditingSpaceName = false }, Cmd.none
    | Error err ->
        { model with IsEditingSpaceName = false; Toast = Some $"Failed to rename space: %s{err}" }, Cmd.none

let handleDeleteSpaceClick (model: Model) : Model * Cmd<Msg> =
    { model with ConfirmDialog = Some { Message = "Leave this space? It will be removed from this device."; Action = ConfirmLeaveSpace } }, Cmd.none

let handleSpaceDeleted (result: Result<unit, string>) (model: Model) : Model * Cmd<Msg> =
    match result with
    | Ok () ->
        let remaining = Storage.getKnownSpaces ()
        match remaining with
        | next :: _ ->
            setActiveSpaceId next.Id
            let cmd = Cmd.OfAsync.perform Storage.rebaseAndDisplay next.Id (fun (state, conflicts) -> SpaceLoaded (next.Id, state, conflicts))
            { model with ShowSettings = false; KnownSpaces = remaining; ActiveSpaceId = Some next.Id }, cmd
        | [] ->
            clearActiveSpaceId ()
            { model with ShowSettings = false; KnownSpaces = []
                         ActiveSpaceId = None; SpaceState = SpaceState.Empty; Page = CreateSpace }, Cmd.none
    | Error err ->
        { model with Toast = Some $"Failed to leave space: %s{err}" }, Cmd.none
