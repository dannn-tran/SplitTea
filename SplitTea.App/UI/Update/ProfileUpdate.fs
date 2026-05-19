module ProfileUpdate

open Elmish
open UITypes
open AppHelpers

let handleStartProfileRename (model: Model) : Model * Cmd<Msg> =
    let name =
        resolveActor model
        |> fun id -> model.SpaceState.Members |> Map.tryFind id
        |> Option.map _.DisplayName
        |> Option.defaultValue ""
    { model with IsEditingProfileName = true; ProfileNameText = name; ProfileNameError = None }, Cmd.none

let handleProfileNameTextSet (text: string) (model: Model) : Model * Cmd<Msg> =
    { model with ProfileNameText = text; ProfileNameError = None }, Cmd.none

let handleSaveProfileRename (model: Model) : Model * Cmd<Msg> =
    match model.ActiveSpaceId with
    | None -> model, Cmd.none
    | Some sid ->
        let name = model.ProfileNameText.Trim()
        if name = "" then
            { model with ProfileNameError = Some "Name is required." }, Cmd.none
        else
            let actorId = resolveActor model
            let cmd =
                Cmd.OfAsync.either
                    (fun () -> Commands.renameMember sid actorId actorId name)
                    ()
                    (fun () -> ProfileNameSaved (Ok ()))
                    (fun ex -> ProfileNameSaved (Error ex.Message))
            { model with ProfileNameError = None }, cmd

let handleProfileNameSaved (result: Result<unit, string>) (model: Model) : Model * Cmd<Msg> =
    match result with
    | Ok () ->
        match model.ActiveSpaceId with
        | Some sid ->
            let cmd = Cmd.OfAsync.perform Storage.rebaseAndDisplay sid SpaceRebased
            { model with IsEditingProfileName = false; ProfileNameText = "" }, cmd
        | None -> { model with IsEditingProfileName = false }, Cmd.none
    | Error err ->
        { model with ProfileNameError = Some err }, Cmd.none
