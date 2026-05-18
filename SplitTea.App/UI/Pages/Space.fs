module SpacePage

open Feliz
open SplitTea.Core


let private memberName (state: SpaceState) (id: MemberId) =
    state.Members
    |> Map.tryFind id
    |> Option.map (fun m -> m.DisplayName)
    |> Option.defaultValue "Unknown"

let private formatAmount (currency: string) (amount: decimal) =
    $"%s{currency} %.2f{amount}"

let private formatDate (d: System.DateOnly) =
    $"%04d{d.Year}-%02d{d.Month}-%02d{d.Day}"

let private balanceRow (state: SpaceState) (pos: NetPosition) =
    let name = memberName state pos.MemberId
    let currency = state.Currency
    let color, label =
        if pos.Amount > 0m     then "text-green-600", $"gets back %s{formatAmount currency pos.Amount}"
        elif pos.Amount < 0m   then "text-red-600", $"owes %s{formatAmount currency -pos.Amount}"
        else                        "text-gray-500",  "settled up"
    Html.div [
        prop.className "flex justify-between items-center py-2 border-b border-gray-100 last:border-0"
        prop.children [
            Html.span [ prop.className "font-medium text-gray-800"; prop.text name ]
            Html.span [ prop.className $"text-sm %s{color}"; prop.text label ]
        ]
    ]

let private settlementRow (state: SpaceState) (dispatch: UITypes.Msg -> unit) (s: SuggestedSettlement) =
    let currency = state.Currency
    Html.div [
        prop.className "flex items-center gap-2 py-2 text-sm text-gray-700 border-b border-gray-100 last:border-0 cursor-pointer hover:bg-gray-50 rounded-lg px-1 -mx-1 transition-colors"
        prop.onClick (fun _ -> dispatch (UITypes.SettlementFromSuggestion s))
        prop.children [
            Html.span [ prop.className "font-medium"; prop.text (memberName state s.From) ]
            Html.span [ prop.className "text-gray-400"; prop.text "→" ]
            Html.span [ prop.className "font-medium"; prop.text (memberName state s.To) ]
            Html.span [ prop.className "ml-auto font-semibold text-gray-800"; prop.text (formatAmount currency s.Amount) ]
        ]
    ]

let private expenseRow (state: SpaceState) (dispatch: UITypes.Msg -> unit) (expense: ExpenseState) =
    Html.div [
        prop.className "py-3 border-b border-gray-100 last:border-0 space-y-1"
        prop.children [
            Html.div [
                prop.className "flex items-center justify-between gap-3"
                prop.children [
                    Html.span [ prop.className "font-medium text-gray-800 flex-1 min-w-0 truncate"; prop.text expense.Description ]
                    Html.span [ prop.className "font-semibold text-gray-900 shrink-0"; prop.text (formatAmount expense.PaidCurrency expense.PaidAmount) ]
                ]
            ]
            Html.div [
                prop.className "flex items-center gap-3 text-sm text-gray-500"
                prop.children [
                    Html.span [ prop.className "flex-1"; prop.text (memberName state expense.PaidBy) ]
                    Html.span [ prop.text (formatDate expense.Date) ]
                    Html.button [
                        prop.type' "button"
                        prop.className Styles.btnIconSmPrimary
                        prop.title "Edit"
                        prop.onClick (fun _ -> dispatch (UITypes.EditExpenseClick expense.ExpenseId))
                        prop.children [ Icons.pencil ]
                    ]
                    Html.button [
                        prop.type' "button"
                        prop.className Styles.btnIconSmDanger
                        prop.title "Delete"
                        prop.onClick (fun _ -> dispatch (UITypes.DeleteExpenseClick expense.ExpenseId))
                        prop.children [ Icons.trash ]
                    ]
                ]
            ]
            match expense.Category with
            | Some category ->
                Html.span [
                    prop.className "inline-flex text-xs font-medium rounded-full bg-teal-50 text-teal-700 px-2 py-1"
                    prop.text category
                ]
            | None -> ()
        ]
    ]

let private settingsModal (state: SpaceState) (model: UITypes.Model) (dispatch: UITypes.Msg -> unit) =
    let categories =
        state.Categories
        |> Map.toList
        |> List.map snd
        |> List.filter (fun c -> not c.IsArchived)
        |> List.sortBy (fun c -> c.Name)

    Html.div [
        prop.className "fixed inset-0 z-50 flex items-end sm:items-center justify-center"
        prop.children [
            Html.div [
                prop.className "absolute inset-0 bg-black/40"
                prop.onClick (fun _ -> dispatch UITypes.SettingsToggled)
            ]
            Html.div [
                prop.className "relative z-10 bg-white rounded-t-2xl sm:rounded-2xl w-full sm:max-w-md max-h-[80vh] overflow-y-auto p-5 space-y-5"
                prop.children [
                    Html.div [
                        prop.className "flex items-center justify-between"
                        prop.children [
                            Html.h2 [ prop.className "text-lg font-bold text-gray-900"; prop.text "Space Settings" ]
                            Html.button [
                                prop.type' "button"
                                prop.className "p-1 text-gray-400 hover:text-gray-600 rounded-lg transition-colors text-lg leading-none"
                                prop.onClick (fun _ -> dispatch UITypes.SettingsToggled)
                                prop.text "✕"
                            ]
                        ]
                    ]
                    Html.div [
                        prop.className "space-y-2"
                        prop.children [
                            Html.h3 [ prop.className Styles.sectionHeading; prop.text "Space name" ]
                            if model.IsEditingSpaceName then
                                Html.div [
                                    prop.className "flex gap-2 items-center"
                                    prop.children [
                                        Html.input [
                                            prop.className Styles.inputFlex
                                            prop.value model.SpaceNameText
                                            prop.autoFocus true
                                            prop.onChange (UITypes.SpaceNameTextSet >> dispatch)
                                            prop.onKeyDown (fun e ->
                                                if e.key = "Enter" then dispatch UITypes.SaveSpaceRename)
                                        ]
                                        Html.button [
                                            prop.type' "button"
                                            prop.className Styles.btnIconSmPrimary
                                            prop.title "Save"
                                            prop.onClick (fun _ -> dispatch UITypes.SaveSpaceRename)
                                            prop.children [ Icons.check ]
                                        ]
                                    ]
                                ]
                            else
                                Html.div [
                                    prop.className "flex items-center gap-2"
                                    prop.children [
                                        Html.span [ prop.className "flex-1 text-sm text-gray-800"; prop.text state.Name ]
                                        Html.button [
                                            prop.type' "button"
                                            prop.className Styles.btnIconSmPrimary
                                            prop.title "Rename"
                                            prop.onClick (fun _ -> dispatch UITypes.StartSpaceRename)
                                            prop.children [ Icons.pencil ]
                                        ]
                                    ]
                                ]
                        ]
                    ]
                    Html.div [
                        prop.className "space-y-2"
                        prop.children [
                            Html.h3 [ prop.className Styles.sectionHeading; prop.text "Categories" ]
                            Html.div [
                                prop.className "flex gap-2"
                                prop.children [
                                    Html.input [
                                        prop.className Styles.inputFlex
                                        prop.placeholder "Add category"
                                        prop.value model.NewCategory
                                        prop.onChange (UITypes.NewCategorySet >> dispatch)
                                        prop.onKeyDown (fun e -> if e.key = "Enter" then dispatch UITypes.AddCategorySubmit)
                                    ]
                                    Html.button [
                                        prop.className Styles.btnInlinePrimary
                                        prop.disabled model.IsAuthLoading
                                        prop.text "Add"
                                        prop.onClick (fun _ -> dispatch UITypes.AddCategorySubmit)
                                    ]
                                ]
                            ]
                            match model.CategoryError with
                            | Some err -> Html.p [ prop.className Styles.error; prop.text err ]
                            | None -> ()
                            if List.isEmpty categories then
                                Html.p [ prop.className "text-sm text-gray-400 italic"; prop.text "No categories yet." ]
                            else
                                Html.div [
                                    prop.className "space-y-2"
                                    prop.children (
                                        categories |> List.map (fun category ->
                                            let isEditing = model.EditingCategory = Some category.Name
                                            Html.div [
                                                prop.className "flex items-center gap-2"
                                                prop.children [
                                                    if isEditing then
                                                        Html.input [
                                                            prop.className Styles.inputFlex
                                                            prop.value model.EditCategoryName
                                                            prop.onChange (UITypes.EditCategoryNameSet >> dispatch)
                                                            prop.onKeyDown (fun e ->
                                                                if e.key = "Enter" then dispatch UITypes.SaveCategoryRename)
                                                        ]
                                                    else
                                                        Html.span [ prop.className "flex-1 text-sm text-gray-800"; prop.text category.Name ]
                                                    Html.button [
                                                        prop.type' "button"
                                                        prop.className Styles.btnIconSmPrimary
                                                        prop.title (if isEditing then "Save" else "Rename")
                                                        prop.onClick (fun _ ->
                                                            if isEditing then dispatch UITypes.SaveCategoryRename
                                                            else dispatch (UITypes.StartCategoryRename category.Name))
                                                        prop.children [ if isEditing then Icons.check else Icons.pencil ]
                                                    ]
                                                    if not isEditing then
                                                        Html.button [
                                                            prop.type' "button"
                                                            prop.className Styles.btnIconSmDanger
                                                            prop.title "Archive"
                                                            prop.onClick (fun _ -> dispatch (UITypes.ArchiveCategory category.Name))
                                                            prop.children [ Icons.trash ]
                                                        ]
                                                ]
                                            ])
                                    )
                                ]
                        ]
                    ]
                    Html.div [
                        prop.className "border-t border-gray-100 pt-3"
                        prop.children [
                            Html.button [
                                prop.type' "button"
                                prop.className "w-full text-sm text-red-600 hover:text-red-800 font-medium py-2 transition-colors"
                                prop.text "Leave Space"
                                prop.onClick (fun _ -> dispatch UITypes.DeleteSpaceClick)
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let view (state: SpaceState) (model: UITypes.Model) (dispatch: UITypes.Msg -> unit) =
    let positions   = Projections.computeNetPositions state
    let settlements = Projections.computeMinimumSettlements positions
    let spaceName   = if state.Name = "" then "Space" else state.Name
    let categories =
        state.Categories
        |> Map.toList
        |> List.map snd
        |> List.filter (fun c -> not c.IsArchived)
        |> List.sortBy (fun c -> c.Name)
    let expenses =
        state.Expenses
        |> Map.toList
        |> List.map snd
        |> List.filter (fun e -> not e.IsDeleted)
        |> List.filter (fun e -> model.CategoryFilter = "" || e.Category = Some model.CategoryFilter)
        |> List.sortByDescending (fun e -> e.Date, e.Description)

    Html.div [
        prop.children [
            Html.div [
                prop.className "max-w-lg mx-auto px-4 pt-8 pb-20 space-y-6"
                prop.children [
                    Html.div [
                        prop.className "flex items-center justify-between"
                        prop.children [
                            Html.button [
                                prop.type' "button"
                                prop.className "flex items-center gap-1.5 text-2xl font-bold text-gray-900 hover:text-teal-700 transition-colors"
                                prop.onClick (fun _ -> dispatch UITypes.SpaceSwitcherToggled)
                                prop.children [
                                    Html.span [ prop.text spaceName ]
                                    Icons.chevronDown
                                ]
                            ]
                            Html.div [
                                prop.className "flex items-center gap-1"
                                prop.children [
                                    Html.button [
                                        prop.type' "button"
                                        prop.className Styles.btnIcon
                                        prop.title "Add expense"
                                        prop.onClick (fun _ -> dispatch UITypes.AddExpenseClick)
                                        prop.children [ Icons.plus ]
                                    ]
                                    Html.button [
                                        prop.type' "button"
                                        prop.className Styles.btnIcon
                                        prop.title "Space settings"
                                        prop.onClick (fun _ -> dispatch UITypes.SettingsToggled)
                                        prop.children [ Icons.gear ]
                                    ]
                                ]
                            ]
                        ]
                    ]

                    Html.div [
                        prop.className (Styles.cx [Styles.card; "p-4"])
                        prop.children [
                            Html.h2 [ prop.className (Styles.cx [Styles.sectionHeading; "mb-3"]); prop.text "Balances" ]
                            Html.div (positions |> List.map (balanceRow state))
                        ]
                    ]

                    Html.div [
                        prop.className (Styles.cx [Styles.card; "p-4"])
                        prop.children [
                            Html.div [
                                prop.className "flex items-center justify-between mb-1"
                                prop.children [
                                    Html.h2 [ prop.className Styles.sectionHeading; prop.text "Suggested Settlements" ]
                                    Html.button [
                                        prop.type' "button"
                                        prop.className Styles.linkPrimary
                                        prop.text "Record settlement"
                                        prop.onClick (fun _ -> dispatch UITypes.RecordSettlementClick)
                                    ]
                                ]
                            ]
                            if List.isEmpty settlements then
                                Html.p [ prop.className "text-sm text-gray-400 italic mt-2"; prop.text "Everyone is settled up." ]
                            else
                                Html.div [
                                    prop.children [
                                        Html.p [ prop.className "text-xs text-gray-400 mb-3"; prop.text "Tap a row to pre-fill the settlement form." ]
                                        Html.div (settlements |> List.map (settlementRow state dispatch))
                                    ]
                                ]
                        ]
                    ]

                    Html.div [
                        prop.className (Styles.cx [Styles.card; "p-4 space-y-4"])
                        prop.children [
                            Html.div [
                                prop.className "flex items-center justify-between gap-3"
                                prop.children [
                                    Html.h2 [ prop.className Styles.sectionHeading; prop.text "Expenses" ]
                                    Html.select [
                                        prop.className Styles.inputBase
                                        prop.value model.CategoryFilter
                                        prop.onChange (UITypes.CategoryFilterSet >> dispatch)
                                        prop.children (
                                            Html.option [ prop.value ""; prop.text "All categories" ]
                                            :: (categories |> List.map (fun category ->
                                                Html.option [ prop.value category.Name; prop.text category.Name ]))
                                        )
                                    ]
                                ]
                            ]
                            if List.isEmpty expenses then
                                Html.p [ prop.className "text-sm text-gray-400 italic"; prop.text "No expenses match this filter." ]
                            else
                                Html.div (expenses |> List.map (expenseRow state dispatch))
                        ]
                    ]
                ]
            ]

            if model.ShowSettings then settingsModal state model dispatch
        ]
    ]
