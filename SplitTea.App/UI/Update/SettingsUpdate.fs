module SettingsUpdate

open Elmish
open Fable.Core.JsInterop
open UITypes
open AppHelpers

let handleSettingsToggled (model: Model) : Model * Cmd<Msg> =
    if model.ShowSettings then
        { model with ShowSettings    = false
                     IsEditingSpaceName = false; SpaceNameText = ""
                     EditingCategory    = None;  EditCategoryName = "" }, Cmd.none
    else
        { model with ShowSettings = true }, Cmd.none

let handleSpaceSwitcherToggled (model: Model) : Model * Cmd<Msg> =
    { model with ShowSpaceSwitcher = not model.ShowSpaceSwitcher }, Cmd.none

let handleRequestConfirm (req: ConfirmRequest) (model: Model) : Model * Cmd<Msg> =
    { model with ConfirmDialog = Some req }, Cmd.none

let handleConfirmResolved (confirmed: bool) (model: Model) : Model * Cmd<Msg> =
    if not confirmed then
        { model with ConfirmDialog = None }, Cmd.none
    else
        match model.ConfirmDialog with
        | None -> model, Cmd.none
        | Some { Action = ConfirmDeleteExpense expenseId } ->
            let model' = { model with ConfirmDialog = None }
            match model.ActiveSpaceId with
            | None -> model', Cmd.none
            | Some sid ->
                let actorId = resolveActor model
                let cmd =
                    Cmd.OfAsync.either
                        (fun () -> Commands.deleteExpense sid actorId expenseId)
                        ()
                        (fun () -> ExpenseDeleted (Ok ()))
                        (fun ex  -> ExpenseDeleted (Error ex.Message))
                model', cmd
        | Some { Action = ConfirmLeaveSpace } ->
            let model' = { model with ConfirmDialog = None }
            match model.ActiveSpaceId with
            | None -> model', Cmd.none
            | Some sid ->
                let cmd =
                    Cmd.OfAsync.either
                        (fun () -> Storage.deleteSpace sid)
                        ()
                        (fun () -> SpaceDeleted (Ok ()))
                        (fun ex  -> SpaceDeleted (Error ex.Message))
                model', cmd

let handleInstallPromptReady (model: Model) : Model * Cmd<Msg> =
    { model with InstallPromptAvailable = true }, Cmd.none

let handleInstallApp (deferredInstallPrompt: obj option ref) (model: Model) : Model * Cmd<Msg> =
    match deferredInstallPrompt.Value with
    | Some prompt ->
        emitJsExpr prompt "$0.prompt()" |> ignore
        deferredInstallPrompt.Value <- None
    | None -> ()
    { model with InstallPromptAvailable = false }, Cmd.none

let handleDismissInstallPrompt (deferredInstallPrompt: obj option ref) (model: Model) : Model * Cmd<Msg> =
    deferredInstallPrompt.Value <- None
    { model with InstallPromptAvailable = false }, Cmd.none
