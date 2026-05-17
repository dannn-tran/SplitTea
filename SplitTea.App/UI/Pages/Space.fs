module SpacePage

open Feliz
open SplitTea.Core

let private svgIcon (attrs: ISvgAttribute list) (paths: ISvgAttribute list list) =
    Svg.svg ([ svg.viewBox (0, 0, 16, 16); svg.fill "currentColor"; svg.className "w-4 h-4" ] @ attrs @
             [ svg.children (paths |> List.map Svg.path) ])

let private pencilIcon =
    svgIcon [] [
        [ svg.d "M13.488 2.513a1.75 1.75 0 0 0-2.475 0L6.75 6.774a2.75 2.75 0 0 0-.596.892l-.848 2.047a.75.75 0 0 0 .98.98l2.047-.848a2.75 2.75 0 0 0 .892-.596l4.261-4.262a1.75 1.75 0 0 0 0-2.474Z" ]
        [ svg.d "M4.75 3.5c-.69 0-1.25.56-1.25 1.25v6.5c0 .69.56 1.25 1.25 1.25h6.5c.69 0 1.25-.56 1.25-1.25V9A.75.75 0 0 1 14 9v2.25A2.75 2.75 0 0 1 11.25 14h-6.5A2.75 2.75 0 0 1 2 11.25v-6.5A2.75 2.75 0 0 1 4.75 2H7a.75.75 0 0 1 0 1.5H4.75Z" ]
    ]

let private checkIcon =
    svgIcon [] [
        [ svg.d "M12.207 4.793a1 1 0 0 1 0 1.414l-5 5a1 1 0 0 1-1.414 0l-2-2a1 1 0 0 1 1.414-1.414L6.5 9.086l4.293-4.293a1 1 0 0 1 1.414 0Z" ]
    ]

let private trashIcon =
    svgIcon [] [
        [ svg.custom ("fillRule", "evenodd"); svg.clipRule.evenodd
          svg.d "M5 3.25V4H2.75a.75.75 0 0 0 0 1.5h.3l.815 8.15A1.5 1.5 0 0 0 5.357 15h5.285a1.5 1.5 0 0 0 1.493-1.35l.815-8.15h.3a.75.75 0 0 0 0-1.5H11v-.75A2.25 2.25 0 0 0 8.75 1h-1.5A2.25 2.25 0 0 0 5 3.25Zm2.25-.75a.75.75 0 0 0-.75.75V4h3v-.75a.75.75 0 0 0-.75-.75h-1.5ZM6.05 6a.75.75 0 0 1 .787.713l.275 5.5a.75.75 0 0 1-1.498.075l-.275-5.5A.75.75 0 0 1 6.05 6Zm3.9 0a.75.75 0 0 1 .712.787l-.275 5.5a.75.75 0 0 1-1.498-.075l.275-5.5a.75.75 0 0 1 .786-.711Z" ]
    ]

let private gearIcon =
    svgIcon [] [
        [ svg.custom ("fillRule", "evenodd"); svg.clipRule.evenodd
          svg.d "M8 0a8.2 8.2 0 0 1 .701.031C9.444.095 9.99.645 10.16 1.29l.288 1.107c.018.066.079.158.212.224.231.114.454.243.668.386.123.082.233.09.299.071l1.103-.303c.644-.176 1.392.021 1.82.63.27.385.506.792.704 1.218.315.675.111 1.422-.364 1.891l-.814.806c-.049.048-.098.147-.088.294.016.257.016.515 0 .772-.01.147.038.246.088.294l.814.806c.475.469.679 1.216.364 1.891a7.977 7.977 0 0 1-.704 1.217c-.428.61-1.176.807-1.82.63l-1.102-.302c-.067-.019-.177-.011-.3.071a5.909 5.909 0 0 1-.668.386c-.133.066-.194.158-.211.224l-.29 1.106c-.168.646-.715 1.196-1.458 1.26a8.006 8.006 0 0 1-1.402 0c-.743-.064-1.289-.614-1.458-1.26l-.289-1.106c-.018-.066-.079-.158-.212-.224a5.738 5.738 0 0 1-.668-.386c-.123-.082-.233-.09-.299-.071l-1.103.303c-.644.176-1.392-.021-1.82-.63a8.12 8.12 0 0 1-.704-1.218c-.315-.675-.111-1.422.363-1.891l.815-.806c.05-.048.098-.147.088-.294a6.214 6.214 0 0 1 0-.772c.01-.147-.038-.246-.088-.294l-.815-.806C.635 6.045.431 5.298.746 4.623a7.92 7.92 0 0 1 .704-1.217c.428-.61 1.176-.807 1.82-.63l1.102.302c.067.019.177.011.3-.071.214-.143.437-.272.668-.386.133-.066.194-.158.211-.224l.29-1.106C6.717.645 7.264.095 8.006.031A8.2 8.2 0 0 1 8 0Zm-.571 5.5a2.929 2.929 0 1 0 1.142 5.745A2.929 2.929 0 0 0 7.429 5.5Z" ]
    ]

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
                        prop.children [ pencilIcon ]
                    ]
                    Html.button [
                        prop.type' "button"
                        prop.className Styles.btnIconSmDanger
                        prop.title "Delete"
                        prop.onClick (fun _ -> dispatch (UITypes.DeleteExpenseClick expense.ExpenseId))
                        prop.children [ trashIcon ]
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
                prop.className "relative z-10 bg-white rounded-t-2xl sm:rounded-2xl w-full sm:max-w-md max-h-[80vh] overflow-y-auto p-5 space-y-4"
                prop.children [
                    Html.div [
                        prop.className "flex items-center justify-between"
                        prop.children [
                            Html.h2 [ prop.className "text-lg font-bold text-gray-900"; prop.text "Categories" ]
                            Html.button [
                                prop.type' "button"
                                prop.className "p-1 text-gray-400 hover:text-gray-600 rounded-lg transition-colors text-lg leading-none"
                                prop.onClick (fun _ -> dispatch UITypes.SettingsToggled)
                                prop.text "✕"
                            ]
                        ]
                    ]
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
                                                prop.children [ if isEditing then checkIcon else pencilIcon ]
                                            ]
                                            if not isEditing then
                                                Html.button [
                                                    prop.type' "button"
                                                    prop.className Styles.btnIconSmDanger
                                                    prop.title "Archive"
                                                    prop.onClick (fun _ -> dispatch (UITypes.ArchiveCategory category.Name))
                                                    prop.children [ trashIcon ]
                                                ]
                                        ]
                                    ])
                            )
                        ]
                ]
            ]
        ]
    ]

let view (state: SpaceState) (model: UITypes.Model) (dispatch: UITypes.Msg -> unit) =
    let positions    = Projections.computeNetPositions state
    let settlements  = Projections.computeMinimumSettlements positions
    let spaceName    = if state.Name = "" then "Space" else state.Name
    let categories =
        state.Categories
        |> Map.toList
        |> List.map snd
        |> List.filter (fun c -> not c.IsArchived)
        |> List.sortBy (fun c -> c.Name)
    let categorySpending = Projections.computeSpendingByCategory state
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
                prop.className "max-w-lg mx-auto px-4 pt-8 pb-28 space-y-6"
                prop.children [
                    Html.div [
                        prop.className "flex items-center justify-between"
                        prop.children [
                            Html.h1 [ prop.className "text-2xl font-bold text-gray-900"; prop.text spaceName ]
                            Html.button [
                                prop.type' "button"
                                prop.className Styles.btnIcon
                                prop.title "Manage categories"
                                prop.onClick (fun _ -> dispatch UITypes.SettingsToggled)
                                prop.children [ gearIcon ]
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

                    if not (List.isEmpty settlements) then
                        Html.div [
                            prop.className (Styles.cx [Styles.card; "p-4"])
                            prop.children [
                                Html.h2 [ prop.className (Styles.cx [Styles.sectionHeading; "mb-1"]); prop.text "Suggested Settlements" ]
                                Html.p [ prop.className "text-xs text-gray-400 mb-3"; prop.text "Tap a row to pre-fill the settlement form." ]
                                Html.div (settlements |> List.map (settlementRow state dispatch))
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

                    Html.div [
                        prop.className (Styles.cx [Styles.card; "p-4"])
                        prop.children [
                            Html.div [
                                prop.className "flex items-center justify-between"
                                prop.children [
                                    Html.h2 [ prop.className Styles.sectionHeading; prop.text "Analytics" ]
                                    Html.button [
                                        prop.type' "button"
                                        prop.className Styles.linkPrimary
                                        prop.text (if model.ShowAnalytics then "Hide" else "Show spend by category")
                                        prop.onClick (fun _ -> dispatch UITypes.AnalyticsToggled)
                                    ]
                                ]
                            ]
                            if model.ShowAnalytics then
                                if List.isEmpty categorySpending then
                                    Html.p [ prop.className "text-sm text-gray-400 italic mt-3"; prop.text "No categorized spending yet." ]
                                else
                                    Html.div [
                                        prop.className "mt-3"
                                        prop.children (
                                            categorySpending |> List.map (fun row ->
                                                Html.div [
                                                    prop.className "flex justify-between items-center py-2 border-b border-gray-100 last:border-0"
                                                    prop.children [
                                                        Html.span [ prop.className "text-sm text-gray-800"; prop.text row.Category ]
                                                        Html.span [ prop.className "text-sm font-semibold text-gray-900"; prop.text (formatAmount state.Currency row.Total) ]
                                                    ]
                                                ])
                                        )
                                    ]
                        ]
                    ]
                ]
            ]

            Html.div [
                prop.className "fixed bottom-0 inset-x-0 bg-white border-t border-gray-200 px-4 py-3 flex gap-3 z-40"
                prop.children [
                    Html.button [
                        prop.className Styles.btnBarPrimary
                        prop.text "Add Expense"
                        prop.onClick (fun _ -> dispatch UITypes.AddExpenseClick)
                    ]
                    Html.button [
                        prop.className Styles.btnBarSecondary
                        prop.text "Record Settlement"
                        prop.onClick (fun _ -> dispatch UITypes.RecordSettlementClick)
                    ]
                ]
            ]

            if model.ShowSettings then settingsModal state model dispatch
        ]
    ]
