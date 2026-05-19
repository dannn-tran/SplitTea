module ExpenseFormPage

open Feliz
open SplitTea.Core
open UITypes


let view (state: SpaceState) (rates: Map<string, decimal>) (form: ExpenseForm) (isEditing: bool) (dispatch: Msg -> unit) =
    let members =
        state.Members
        |> Map.toList
        |> List.map snd
        |> List.sortBy _.DisplayName

    let categories =
        state.Categories
        |> Map.toList
        |> List.map snd
        |> List.filter (fun c -> not c.IsArchived)
        |> List.sortBy _.Name

    let set f = dispatch (ExpenseFormSet (f form))
    let isForeignCurrency = form.Currency <> state.Currency
    let splitValid =
        match form.SplitMode with
        | EqualSplit -> not (Set.isEmpty form.Included)
        | CustomSplit ->
            let totalAmt  = form.Amount.Parsed |> Option.defaultValue 0m
            let sharesSum =
                form.CustomAmounts
                |> Map.toList
                |> List.sumBy (fun (_, txt) -> try decimal txt with _ -> 0m)
            totalAmt > 0m && sharesSum = totalAmt
    let disabled = form.IsSubmitting || form.Description.Trim() = "" || form.Amount.Parsed = None
                   || not splitValid

    Html.div [
        prop.className "max-w-lg mx-auto px-4 py-8 space-y-6"
        prop.children [
            Html.div [
                prop.className "flex items-center gap-3"
                prop.children [
                    Html.button [
                        prop.className Styles.btnText
                        prop.text "←"
                        prop.onClick (fun _ -> dispatch (NavigateTo SpaceOverview))
                    ]
                    Html.h1 [ prop.className "text-xl font-bold text-gray-900"; prop.text (if isEditing then "Edit Expense" else "Add Expense") ]
                ]
            ]
            Html.div [
                prop.className (Styles.cx [Styles.card; "p-5 space-y-4"])
                prop.children [
                    Styles.field "Description"
                        (Html.input [
                            prop.type' "text"
                            prop.className Styles.input
                            prop.placeholder "e.g. Dinner"
                            prop.value form.Description
                            prop.onChange (fun v -> set (fun f -> { f with Description = v }))
                        ])
                    Html.div [
                        prop.className "flex gap-2"
                        prop.children [
                            Html.div [
                                prop.className "flex-1"
                                prop.children [
                                    Styles.field "Amount"
                                        (Html.div [ prop.className "space-y-1"; prop.children [
                                            Html.input [
                                                prop.type' "text"
                                                prop.className (if form.Amount.IsError then Styles.cx [Styles.input; "border-red-400 focus:ring-red-400"] else Styles.input)
                                                prop.placeholder "0.00"
                                                prop.value form.Amount.Text
                                                prop.onChange (fun v -> set (fun f -> { f with Amount = Field.parseDecimal v }))
                                            ]
                                            if form.Amount.IsError then
                                                Html.p [ prop.className Styles.error; prop.text "Invalid amount." ]
                                        ]])
                                ]
                            ]
                            Html.div [
                                prop.className "w-28"
                                prop.children [
                                    Styles.field "Currency"
                                        (Html.select [
                                            prop.className Styles.input
                                            prop.value form.Currency
                                            prop.onChange (fun (v: string) ->
                                                set (fun f -> { f with Currency = v; ExchangeRate = Field.parseDecimal (Currencies.prefillRate v state.Currency rates) }))
                                            prop.children (Currencies.options state.Currency)
                                        ])
                                ]
                            ]
                        ]
                    ]
                    if isForeignCurrency then
                        Styles.field $"Exchange rate (%s{form.Currency} → %s{state.Currency})"
                            (Html.div [ prop.className "space-y-1"; prop.children [
                                Html.input [
                                    prop.type' "text"
                                    prop.className (if form.ExchangeRate.IsError then Styles.cx [Styles.input; "border-red-400 focus:ring-red-400"] else Styles.input)
                                    prop.placeholder "e.g. 0.79"
                                    prop.value form.ExchangeRate.Text
                                    prop.onChange (fun v -> set (fun f -> { f with ExchangeRate = Field.parseDecimal v }))
                                ]
                                if form.ExchangeRate.IsError then
                                    Html.p [ prop.className Styles.error; prop.text "Invalid exchange rate." ]
                            ]])
                    Styles.field "Paid by"
                        (Html.select [
                            prop.className Styles.input
                            prop.value (form.PaidById |> fun (MemberId g) -> string g)
                            prop.onChange (fun (v: string) -> set (fun f -> { f with PaidById = MemberId (System.Guid.Parse v) }))
                            prop.children (
                                members |> List.map (fun m ->
                                    let (MemberId g) = m.Id
                                    Html.option [ prop.value (string g); prop.text m.DisplayName ]
                                )
                            )
                        ])
                    Html.div [
                        prop.className "space-y-2"
                        prop.children [
                            Html.label [ prop.className Styles.label; prop.text "Split" ]
                            Html.div [
                                prop.className "flex rounded-lg border border-gray-300 overflow-hidden text-sm font-medium"
                                prop.children [
                                    Html.button [
                                        prop.type' "button"
                                        prop.className (if form.SplitMode = EqualSplit then "flex-1 py-2 bg-teal-600 text-white" else "flex-1 py-2 text-gray-600 hover:bg-gray-50 transition-colors")
                                        prop.text "Equal"
                                        prop.onClick (fun _ -> set (fun f -> { f with SplitMode = EqualSplit }))
                                    ]
                                    Html.button [
                                        prop.type' "button"
                                        prop.className (if form.SplitMode = CustomSplit then "flex-1 py-2 bg-teal-600 text-white border-l border-gray-300" else "flex-1 py-2 text-gray-600 hover:bg-gray-50 transition-colors border-l border-gray-300")
                                        prop.text "Custom"
                                        prop.onClick (fun _ -> set (fun f -> { f with SplitMode = CustomSplit }))
                                    ]
                                ]
                            ]
                            match form.SplitMode with
                            | EqualSplit ->
                                Html.div [
                                    prop.className "space-y-1"
                                    prop.children (
                                        members |> List.map (fun m ->
                                            let included = Set.contains m.Id form.Included
                                            Html.label [
                                                prop.className (Styles.cx [ "flex items-center gap-3 px-3 py-2 rounded-lg cursor-pointer"; if included then "bg-teal-50" else "hover:bg-gray-50" ])
                                                prop.children [
                                                    Html.input [
                                                        prop.type' "checkbox"
                                                        prop.className "w-4 h-4 text-teal-600 rounded border-gray-300 focus:ring-2 focus:ring-teal-500"
                                                        prop.isChecked included
                                                        prop.onChange (fun (v: bool) ->
                                                            set (fun f ->
                                                                let ids = if v then Set.add m.Id f.Included else Set.remove m.Id f.Included
                                                                { f with Included = ids }))
                                                    ]
                                                    Html.span [ prop.className "text-sm text-gray-800 select-none"; prop.text m.DisplayName ]
                                                ]
                                            ])
                                    )
                                ]
                            | CustomSplit ->
                                let totalAmt   = form.Amount.Parsed |> Option.defaultValue 0m
                                let sharesSum  = form.CustomAmounts |> Map.toList |> List.sumBy (fun (_, txt) -> try decimal txt with _ -> 0m)
                                let remaining  = totalAmt - sharesSum
                                Html.div [
                                    prop.className "space-y-2"
                                    prop.children (
                                        (members |> List.map (fun m ->
                                            let txt = form.CustomAmounts |> Map.tryFind m.Id |> Option.defaultValue ""
                                            Html.div [
                                                prop.className "flex items-center gap-3"
                                                prop.children [
                                                    Html.span [ prop.className "w-24 shrink-0 text-sm text-gray-700 truncate"; prop.text m.DisplayName ]
                                                    Html.input [
                                                        prop.type' "text"
                                                        prop.className Styles.input
                                                        prop.placeholder "0.00"
                                                        prop.value txt
                                                        prop.onChange (fun v -> set (fun f -> { f with CustomAmounts = Map.add m.Id v f.CustomAmounts }))
                                                    ]
                                                ]
                                            ]))
                                        @ [ Html.div [
                                                prop.className "flex justify-end pt-1"
                                                prop.children [
                                                    Html.span [
                                                        prop.className (if totalAmt > 0m && remaining = 0m then "text-xs font-medium text-green-600" else "text-xs font-medium text-red-500")
                                                        prop.text (
                                                            if totalAmt <= 0m then ""
                                                            elif remaining = 0m then "✓ Fully allocated"
                                                            else sprintf "Remaining: %.2f" remaining)
                                                    ]
                                                ]
                                            ] ]
                                    )
                                ]
                        ]
                    ]
                    Html.div [
                        prop.className "space-y-1"
                        prop.children [
                            Html.label [ prop.className Styles.label; prop.text "Category" ]
                            if form.IsAddingCategory then
                                Html.div [
                                    prop.className "flex gap-2 items-center"
                                    prop.children [
                                        Html.input [
                                            prop.type' "text"
                                            prop.className Styles.input
                                            prop.placeholder "New category name"
                                            prop.value form.NewCategoryText
                                            prop.autoFocus true
                                            prop.onChange (fun v -> set (fun f -> { f with NewCategoryText = v }))
                                            prop.onKeyDown (fun e ->
                                                if e.key = "Enter" then dispatch AddCategoryFromForm
                                                elif e.key = "Escape" then set (fun f -> { f with IsAddingCategory = false; NewCategoryText = "" }))
                                        ]
                                        Html.button [
                                            prop.type' "button"
                                            prop.className Styles.btnInlinePrimary
                                            prop.text "Add"
                                            prop.onClick (fun _ -> dispatch AddCategoryFromForm)
                                        ]
                                        Html.button [
                                            prop.type' "button"
                                            prop.className Styles.btnInlineSecondary
                                            prop.text "Cancel"
                                            prop.onClick (fun _ -> set (fun f -> { f with IsAddingCategory = false; NewCategoryText = "" }))
                                        ]
                                    ]
                                ]
                            else
                                Html.select [
                                    prop.className Styles.input
                                    prop.value form.Category
                                    prop.onChange (fun (v: string) ->
                                        if v = "__new__" then
                                            set (fun f -> { f with IsAddingCategory = true; Category = "" })
                                        else
                                            set (fun f -> { f with Category = v }))
                                    prop.children (
                                        Html.option [ prop.value ""; prop.text "— None —" ]
                                        :: (categories |> List.map (fun c ->
                                            Html.option [ prop.value c.Name; prop.text c.Name ]))
                                        @ [ Html.option [ prop.value "__new__"; prop.text "+ New category…" ] ]
                                    )
                                ]
                        ]
                    ]
                    Styles.field "Date"
                        (Html.input [
                            prop.type' "date"
                            prop.className Styles.input
                            prop.value form.DateText
                            prop.onChange (fun v -> set (fun f -> { f with DateText = v }))
                        ])
                    Styles.field "Notes (optional)"
                        (Html.textarea [
                            prop.className Styles.input
                            prop.placeholder "Optional notes..."
                            prop.value form.Notes
                            prop.rows 2
                            prop.onChange (fun v -> set (fun f -> { f with Notes = v }))
                        ])
                    match form.Error with
                    | Some err -> Html.p [ prop.className Styles.error; prop.text err ]
                    | None     -> ()
                    Html.button [
                        prop.className Styles.btnPrimary
                        prop.disabled disabled
                        prop.text (if form.IsSubmitting then "Saving..." elif isEditing then "Save Changes" else "Save Expense")
                        prop.onClick (fun _ -> dispatch ExpenseSubmit)
                    ]
                ]
            ]
        ]
    ]
