module ExpenseFormPage

open Feliz
open SplitTea.Core
open UITypes

let private inputCls = "w-full border border-gray-300 rounded-lg px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-teal-500"

let private categories = [
    ""
    "Food & Drink"
    "Transport"
    "Accommodation"
    "Entertainment"
    "Shopping"
    "Utilities"
    "Health"
    "Other"
]

let private field (label: string) (input: ReactElement) : ReactElement =
    Html.div [
        prop.className "space-y-1"
        prop.children [
            Html.label [ prop.className "block text-sm font-medium text-gray-700"; prop.text label ]
            input
        ]
    ]

let private commonCurrencies = [ "AUD"; "CAD"; "CHF"; "CNY"; "EUR"; "GBP"; "HKD"; "JPY"; "MYR"; "NZD"; "SGD"; "USD" ]

let private currencyOptions (groupCurrency: string) =
    let all = if List.contains groupCurrency commonCurrencies then commonCurrencies else groupCurrency :: commonCurrencies
    all |> List.map (fun c -> Html.option [ prop.value c; prop.text c ])

let view (state: GroupState) (rates: Map<string, decimal>) (form: ExpenseForm) (dispatch: Msg -> unit) =
    let members =
        state.Members
        |> Map.toList
        |> List.map snd
        |> List.sortBy (fun m -> m.DisplayName)

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
                        prop.className "text-teal-600 hover:text-teal-800 text-sm font-medium"
                        prop.text "← Back"
                        prop.onClick (fun _ -> dispatch (NavigateTo GroupOverview))
                    ]
                    Html.h1 [ prop.className "text-xl font-bold text-gray-900"; prop.text "Add Expense" ]
                ]
            ]
            Html.div [
                prop.className "bg-white rounded-xl shadow-sm border border-gray-200 p-5 space-y-4"
                prop.children [
                    field "Description"
                        (Html.input [
                            prop.type' "text"
                            prop.className inputCls
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
                                    field "Amount"
                                        (Html.input [
                                            prop.type' "text"
                                            prop.className inputCls
                                            prop.placeholder "0.00"
                                            prop.value form.AmountText
                                            prop.onChange (fun v -> set (fun f -> { f with AmountText = v }))
                                        ])
                                ]
                            ]
                            Html.div [
                                prop.className "w-28"
                                prop.children [
                                    field "Currency"
                                        (Html.select [
                                            prop.className inputCls
                                            prop.value form.Currency
                                            prop.onChange (fun (v: string) ->
                                            let rateText =
                                                FxRates.getRate v state.Currency rates
                                                |> Option.map (fun r -> string r)
                                                |> Option.defaultValue ""
                                            set (fun f -> { f with Currency = v; ExchangeRateText = rateText }))
                                            prop.children (currencyOptions state.Currency)
                                        ])
                                ]
                            ]
                        ]
                    ]
                    if isForeignCurrency then
                        field (sprintf "Exchange rate (%s → %s)" form.Currency state.Currency)
                            (Html.input [
                                prop.type' "text"
                                prop.className inputCls
                                prop.placeholder "e.g. 0.79"
                                prop.value form.ExchangeRateText
                                prop.onChange (fun v -> set (fun f -> { f with ExchangeRateText = v }))
                            ])
                    field "Paid by"
                        (Html.select [
                            prop.className inputCls
                            prop.value (string form.PaidByIndex)
                            prop.onChange (fun (v: string) -> set (fun f -> { f with PaidByIndex = int v }))
                            prop.children (
                                members |> List.mapi (fun i m ->
                                    Html.option [ prop.value (string i); prop.text m.DisplayName ]
                                )
                            )
                        ])
                    field "Category"
                        (Html.select [
                            prop.className inputCls
                            prop.value form.Category
                            prop.onChange (fun (v: string) -> set (fun f -> { f with Category = v }))
                            prop.children (
                                categories |> List.map (fun c ->
                                    Html.option [
                                        prop.value c
                                        prop.text (if c = "" then "— None —" else c)
                                    ]
                                )
                            )
                        ])
                    field "Date"
                        (Html.input [
                            prop.type' "date"
                            prop.className inputCls
                            prop.value form.DateText
                            prop.onChange (fun v -> set (fun f -> { f with DateText = v }))
                        ])
                    field "Notes (optional)"
                        (Html.textarea [
                            prop.className inputCls
                            prop.placeholder "Optional notes..."
                            prop.value form.Notes
                            prop.rows 2
                            prop.onChange (fun v -> set (fun f -> { f with Notes = v }))
                        ])
                    let sortedContexts = state.Contexts |> Map.toList |> List.map snd |> List.sortBy (fun c -> c.Name)
                    if not (List.isEmpty sortedContexts) then
                        field "Context (optional)"
                            (Html.select [
                                prop.className inputCls
                                prop.value (string form.ContextIndex)
                                prop.onChange (fun (v: string) -> set (fun f -> { f with ContextIndex = int v }))
                                prop.children (
                                    Html.option [ prop.value "0"; prop.text "— None —" ]
                                    :: (sortedContexts |> List.mapi (fun i c ->
                                        Html.option [ prop.value (string (i + 1)); prop.text c.Name ]
                                    ))
                                )
                            ])
                    match form.Error with
                    | Some err -> Html.p [ prop.className "text-sm text-red-600"; prop.text err ]
                    | None     -> ()
                    Html.button [
                        prop.className "w-full bg-teal-600 hover:bg-teal-700 disabled:opacity-50 text-white font-semibold py-3 rounded-xl transition-colors"
                        prop.disabled disabled
                        prop.text (if form.IsSubmitting then "Saving..." else "Save Expense")
                        prop.onClick (fun _ -> dispatch ExpenseSubmit)
                    ]
                ]
            ]
        ]
    ]
