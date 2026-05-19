module CategoryUpdate

open Elmish
open SplitTea.Core
open UITypes
open AppHelpers

let handleCategoryFilterSet (category: string) (model: Model) : Model * Cmd<Msg> =
    { model with CategoryFilter = category }, Cmd.none

let handleNewCategorySet (name: string) (model: Model) : Model * Cmd<Msg> =
    { model with NewCategory = name; CategoryError = None }, Cmd.none

let handleStartCategoryRename (name: string) (model: Model) : Model * Cmd<Msg> =
    { model with EditingCategory = Some name; EditCategoryName = name; CategoryError = None }, Cmd.none

let handleEditCategoryNameSet (name: string) (model: Model) : Model * Cmd<Msg> =
    { model with EditCategoryName = name; CategoryError = None }, Cmd.none

let handleAddCategorySubmit (model: Model) : Model * Cmd<Msg> =
    match model.ActiveSpaceId with
    | None -> model, Cmd.none
    | Some sid ->
        let name = model.NewCategory.Trim()
        if name = "" then
            { model with CategoryError = Some "Category name is required." }, Cmd.none
        elif model.SpaceState.Categories |> Map.containsKey name then
            { model with CategoryError = Some "Category already exists." }, Cmd.none
        else
            let actorId = resolveActor model
            let cmd =
                Cmd.OfAsync.either
                    (fun () -> Commands.addCategory sid actorId name)
                    ()
                    (fun () -> CategorySaved (Ok ()))
                    (fun ex -> CategorySaved (Error ex.Message))
            { model with IsAuthLoading = true; CategoryError = None }, cmd

let handleSaveCategoryRename (model: Model) : Model * Cmd<Msg> =
    match model.ActiveSpaceId, model.EditingCategory with
    | Some sid, Some oldName ->
        let newName = model.EditCategoryName.Trim()
        if newName = "" then
            { model with CategoryError = Some "Category name is required." }, Cmd.none
        elif oldName <> newName && model.SpaceState.Categories |> Map.containsKey newName then
            { model with CategoryError = Some "Category already exists." }, Cmd.none
        else
            let actorId = resolveActor model
            let cmd =
                Cmd.OfAsync.either
                    (fun () -> Commands.renameCategory sid actorId oldName newName)
                    ()
                    (fun () -> CategorySaved (Ok ()))
                    (fun ex -> CategorySaved (Error ex.Message))
            { model with IsAuthLoading = true; CategoryError = None }, cmd
    | _ -> model, Cmd.none

let handleArchiveCategory (name: string) (model: Model) : Model * Cmd<Msg> =
    match model.ActiveSpaceId with
    | None -> model, Cmd.none
    | Some sid ->
        let actorId = resolveActor model
        let cmd =
            Cmd.OfAsync.either
                (fun () -> Commands.archiveCategory sid actorId name)
                ()
                (fun () -> CategorySaved (Ok ()))
                (fun ex -> CategorySaved (Error ex.Message))
        { model with IsAuthLoading = true; CategoryError = None }, cmd

let handleCategorySaved (result: Result<unit, string>) (model: Model) : Model * Cmd<Msg> =
    match result with
    | Ok () ->
        match model.ActiveSpaceId with
        | Some sid ->
            let cmd = Cmd.OfAsync.perform Storage.rebaseAndDisplay sid SpaceRebased
            { model with
                IsAuthLoading    = false
                NewCategory      = ""
                EditingCategory  = None
                EditCategoryName = ""
                CategoryError    = None }, cmd
        | None ->
            { model with IsAuthLoading = false }, Cmd.none
    | Error err ->
        { model with IsAuthLoading = false; CategoryError = Some err }, Cmd.none

let handleAddCategoryFromForm (model: Model) : Model * Cmd<Msg> =
    match model.ActiveSpaceId with
    | None -> model, Cmd.none
    | Some sid ->
        let name = model.ExpenseForm.NewCategoryText.Trim()
        if name = "" then model, Cmd.none
        elif model.SpaceState.Categories |> Map.containsKey name then
            let form = { model.ExpenseForm with Category = name; IsAddingCategory = false; NewCategoryText = "" }
            { model with ExpenseForm = form }, Cmd.none
        else
            let actorId = resolveActor model
            let cmd =
                Cmd.OfAsync.either
                    (fun () -> Commands.addCategory sid actorId name)
                    ()
                    (fun () -> CategoryFromFormSaved (Ok name))
                    (fun ex -> CategoryFromFormSaved (Error ex.Message))
            let form = { model.ExpenseForm with IsAddingCategory = false; NewCategoryText = "" }
            { model with ExpenseForm = form; IsAuthLoading = true }, cmd

let handleCategoryFromFormSaved (result: Result<string, string>) (model: Model) : Model * Cmd<Msg> =
    match result with
    | Ok name ->
        match model.ActiveSpaceId with
        | Some sid ->
            let cmd = Cmd.OfAsync.perform Storage.rebaseAndDisplay sid SpaceRebased
            { model with IsAuthLoading = false; ExpenseForm = { model.ExpenseForm with Category = name } }, cmd
        | None ->
            { model with IsAuthLoading = false }, Cmd.none
    | Error err ->
        { model with IsAuthLoading = false; ExpenseForm = { model.ExpenseForm with Error = Some err } }, Cmd.none
