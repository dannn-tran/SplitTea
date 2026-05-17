module SpacePage

open Feliz
open SplitTea.Core

let private memberName (state: SpaceState) (id: MemberId) =
    state.Members
    |> Map.tryFind id
    |> Option.map (fun m -> m.DisplayName)
    |> Option.defaultValue "Unknown"

let private formatAmount (currency: string) (amount: decimal) =
    sprintf "%s %.2f" currency amount

let private formatDate (d: System.DateOnly) =
    sprintf "%04d-%02d-%02d" d.Year d.Month d.Day

let private balanceRow (state: SpaceState) (pos: NetPosition) =
    let name = memberName state pos.MemberId
    let currency = state.Currency
    let (color, label) =
        if pos.Amount > 0m      then "text-green-600", sprintf "gets back %s" (formatAmount currency pos.Amount)
        elif pos.Amount < 0m   then "text-red-600",   sprintf "owes %s"      (formatAmount currency -pos.Amount)
        else                        "text-gray-500",  "settled up"
    Html.div [
        prop.className "flex justify-between items-center py-2 border-b border-gray-100 last:border-0"
        prop.children [
            Html.span [ prop.className "font-medium text-gray-800"; prop.text name ]
            Html.span [ prop.className (sprintf "text-sm %s" color); prop.text label ]
        ]
    ]

let private settlementRow (state: SpaceState) (s: SuggestedSettlement) =
    let currency = state.Currency
    Html.div [
        prop.className "flex items-center gap-2 py-2 text-sm text-gray-700 border-b border-gray-100 last:border-0"
        prop.children [
            Html.span [ prop.className "font-medium"; prop.text (memberName state s.From) ]
            Html.span [ prop.className "text-gray-400"; prop.text "->" ]
            Html.span [ prop.className "font-medium"; prop.text (memberName state s.To) ]
            Html.span [ prop.className "ml-auto font-semibold text-gray-800"; prop.text (formatAmount currency s.Amount) ]
        ]
    ]

let private expenseRow (state: SpaceState) (expense: ExpenseState) =
    Html.div [
        prop.className "py-3 border-b border-gray-100 last:border-0 space-y-1"
        prop.children [
            Html.div [
                prop.className "flex items-center justify-between gap-3"
                prop.children [
                    Html.span [ prop.className "font-medium text-gray-800"; prop.text expense.Description ]
                    Html.span [ prop.className "font-semibold text-gray-900"; prop.text (formatAmount expense.PaidCurrency expense.PaidAmount) ]
                ]
            ]
            Html.div [
                prop.className "flex items-center justify-between gap-3 text-sm text-gray-500"
                prop.children [
                    Html.span [ prop.text (memberName state expense.PaidBy) ]
                    Html.span [ prop.text (formatDate expense.Date) ]
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
        prop.className "max-w-lg mx-auto px-4 py-8 space-y-6"
        prop.children [
            Html.h1 [ prop.className "text-2xl font-bold text-gray-900"; prop.text spaceName ]

            Html.div [
                prop.className "bg-white rounded-xl shadow-sm border border-gray-200 p-4"
                prop.children [
                    Html.h2 [ prop.className "text-sm font-semibold text-gray-500 uppercase tracking-wide mb-3"; prop.text "Balances" ]
                    Html.div (positions |> List.map (balanceRow state))
                ]
            ]

            if not (List.isEmpty settlements) then
                Html.div [
                    prop.className "bg-white rounded-xl shadow-sm border border-gray-200 p-4"
                    prop.children [
                        Html.h2 [ prop.className "text-sm font-semibold text-gray-500 uppercase tracking-wide mb-3"; prop.text "Suggested Settlements" ]
                        Html.div (settlements |> List.map (settlementRow state))
                    ]
                ]

            Html.div [
                prop.className "bg-white rounded-xl shadow-sm border border-gray-200 p-4 space-y-4"
                prop.children [
                    Html.div [
                        prop.className "flex items-center justify-between gap-3"
                        prop.children [
                            Html.h2 [ prop.className "text-sm font-semibold text-gray-500 uppercase tracking-wide"; prop.text "Categories" ]
                            Html.span [ prop.className "text-xs text-gray-400"; prop.text "Space-wide" ]
                        ]
                    ]
                    Html.div [
                        prop.className "flex gap-2"
                        prop.children [
                            Html.input [
                                prop.className "flex-1 border border-gray-300 rounded-lg px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-teal-500"
                                prop.placeholder "Add category"
                                prop.value model.NewCategory
                                prop.onChange (UITypes.NewCategorySet >> dispatch)
                            ]
                            Html.button [
                                prop.className "bg-teal-600 hover:bg-teal-700 disabled:opacity-50 text-white font-semibold px-4 rounded-lg transition-colors"
                                prop.disabled model.IsAuthLoading
                                prop.text "Add"
                                prop.onClick (fun _ -> dispatch UITypes.AddCategorySubmit)
                            ]
                        ]
                    ]
                    match model.CategoryError with
                    | Some err -> Html.p [ prop.className "text-sm text-red-600"; prop.text err ]
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
                                                    prop.className "flex-1 border border-gray-300 rounded-lg px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-teal-500"
                                                    prop.value model.EditCategoryName
                                                    prop.onChange (UITypes.EditCategoryNameSet >> dispatch)
                                                ]
                                            else
                                                Html.span [ prop.className "flex-1 text-sm text-gray-800"; prop.text category.Name ]
                                            Html.button [
                                                prop.className "text-xs text-teal-600 hover:text-teal-800 font-medium"
                                                prop.text (if isEditing then "Save" else "Rename")
                                                prop.onClick (fun _ ->
                                                    if isEditing then dispatch UITypes.SaveCategoryRename
                                                    else dispatch (UITypes.StartCategoryRename category.Name))
                                            ]
                                            Html.button [
                                                prop.className "text-xs text-red-500 hover:text-red-700 font-medium"
                                                prop.text "Archive"
                                                prop.onClick (fun _ -> dispatch (UITypes.ArchiveCategory category.Name))
                                            ]
                                        ]
                                    ])
                            )
                        ]
                ]
            ]

            Html.div [
                prop.className "bg-white rounded-xl shadow-sm border border-gray-200 p-4 space-y-4"
                prop.children [
                    Html.div [
                        prop.className "flex items-center justify-between gap-3"
                        prop.children [
                            Html.h2 [ prop.className "text-sm font-semibold text-gray-500 uppercase tracking-wide"; prop.text "Expenses" ]
                            Html.select [
                                prop.className "border border-gray-300 rounded-lg px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-teal-500"
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
                        Html.div (expenses |> List.map (expenseRow state))
                ]
            ]

            Html.div [
                prop.className "bg-white rounded-xl shadow-sm border border-gray-200 p-4"
                prop.children [
                    Html.h2 [ prop.className "text-sm font-semibold text-gray-500 uppercase tracking-wide mb-3"; prop.text "Spend by Category" ]
                    if List.isEmpty categorySpending then
                        Html.p [ prop.className "text-sm text-gray-400 italic"; prop.text "No categorized spending yet." ]
                    else
                        Html.div [
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

            Html.div [
                prop.className "flex gap-3"
                prop.children [
                    Html.button [
                        prop.className "flex-1 bg-teal-600 hover:bg-teal-700 text-white font-semibold py-3 rounded-xl transition-colors"
                        prop.text "Add Expense"
                        prop.onClick (fun _ -> dispatch UITypes.AddExpenseClick)
                    ]
                    Html.button [
                        prop.className "flex-1 bg-gray-100 hover:bg-gray-200 text-gray-700 font-semibold py-3 rounded-xl transition-colors"
                        prop.text "Record Settlement"
                        prop.onClick (fun _ -> dispatch UITypes.RecordSettlementClick)
                    ]
                ]
            ]
        ]
    ]
