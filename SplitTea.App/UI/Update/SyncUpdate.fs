module SyncUpdate

open Elmish
open SplitTea.Core
open UITypes

let handleSpaceStateUpdated (ss: SpaceState) (model: Model) : Model * Cmd<Msg> =
    { model with SpaceState = ss }, Cmd.none

let handleRemoteEventReceived (sid: SpaceId) (model: Model) : Model * Cmd<Msg> =
    match model.ActiveSpaceId with
    | Some active when active = sid ->
        let cmd = Cmd.OfAsync.perform Storage.rebaseAndDisplay sid SpaceRebased
        model, cmd
    | _ -> model, Cmd.none

let handleSyncOutcome (sid: SpaceId) (outcomes: Sync.PushOutcome list) (model: Model) : Model * Cmd<Msg> =
    let needsRebase = outcomes |> List.exists (fun o -> o = Sync.PermanentRejection)
    let cmd =
        if needsRebase then Cmd.OfAsync.perform Storage.rebaseAndDisplay sid SpaceRebased
        else Cmd.none
    model, cmd

let handleFlushPending (model: Model) : Model * Cmd<Msg> =
    match model.ActiveSpaceId, model.Auth with
    | Some sid, Some user ->
        let cmd = Cmd.OfAsync.perform (fun () -> Sync.pushPending user.AccessToken) () (fun outcomes -> SyncOutcome (sid, outcomes))
        model, cmd
    | _ -> model, Cmd.none

let handleDismissConflict (eventId: EventId) (model: Model) : Model * Cmd<Msg> =
    let (EventId g) = eventId
    let cmd = Cmd.OfAsync.attempt (fun () -> IndexedDb.deleteEvent (string g)) () (fun _ -> SyncDone)
    { model with Conflicts = model.Conflicts |> List.filter (fun c -> c.EventId <> eventId) }, cmd

let handleExchangeRatesLoaded (rates: Map<string, decimal>) (model: Model) : Model * Cmd<Msg> =
    { model with ExchangeRates = rates }, Cmd.none

let handleToastCleared (model: Model) : Model * Cmd<Msg> =
    { model with Toast = None }, Cmd.none
