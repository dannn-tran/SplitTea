module ExpenseFormPage

open Feliz
open SplitTea.Core
open UITypes

let private commonCurrencies = [ "AUD"; "CAD"; "CHF"; "CNY"; "EUR"; "GBP"; "HKD"; "JPY"; "MYR"; "NZD"; "SGD"; "USD" ]

let private currencyOptions (groupCurrency: string) =
    let all = if List.contains groupCurrency commonCurrencies then commonCurrencies else groupCurrency :: commonCurrencies
    all |> List.map (fun c -> Html.option [ prop.value c; prop.text c ])

let view (state: SpaceState) (rates: Map<string, decimal>) (form: ExpenseForm) (dispatch: Msg -> unit) =
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
    let disabled = form.IsSubmitting || form.Description.Trim() = "" || form.AmountText.Trim() = ""
                   || (isForeignCurrency && form.ExchangeRateText.Trim() = "")

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
                    Html.h1 [ prop.className "text-xl font-bold text-gray-900"; prop.text "Add Expense" ]
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
                                        (Html.input [
                                            prop.type' "text"
                                            prop.className Styles.input
                                            prop.placeholder "0.00"
                                            prop.value form.AmountText
                                            prop.onChange (fun v -> set (fun f -> { f with AmountText = v }))
                                        ])
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
                                                let rateText =
                                                    FxRates.getRate v state.Currency rates
                                                    |> Option.map string
                                                    |> Option.defaultValue ""
                                                set (fun f -> { f with Currency = v; ExchangeRateText = rateText }))
                                            prop.children (currencyOptions state.Currency)
                                        ])
                                ]
                            ]
                        ]
                    ]
                    if isForeignCurrency then
                        Styles.field $"Exchange rate (%s{form.Currency} → %s{state.Currency})"
                            (Html.input [
                                prop.type' "text"
                                prop.className Styles.input
                                prop.placeholder "e.g. 0.79"
                                prop.value form.ExchangeRateText
                                prop.onChange (fun v -> set (fun f -> { f with ExchangeRateText = v }))
                            ])
                    Styles.field "Paid by"
                        (Html.select [
                            prop.className Styles.input
                            prop.value (string form.PaidByIndex)
                            prop.onChange (fun (v: string) -> set (fun f -> { f with PaidByIndex = int v }))
                            prop.children (
                                members |> List.mapi (fun i m ->
                                    Html.option [ prop.value (string i); prop.text m.DisplayName ]
                                )
                            )
                        ])
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
                        prop.text (if form.IsSubmitting then "Saving..." else "Save Expense")
                        prop.onClick (fun _ -> dispatch ExpenseSubmit)
                    ]
                ]
            ]
        ]
    ]
