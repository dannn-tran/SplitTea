module SettlementFormPage

open Feliz
open SplitTea.Core
open UITypes

let private inputCls = "w-full border border-gray-300 rounded-lg px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-teal-500"

let private commonCurrencies = [ "AUD"; "CAD"; "CHF"; "CNY"; "EUR"; "GBP"; "HKD"; "JPY"; "MYR"; "NZD"; "SGD"; "USD" ]

let private currencyOptions (groupCurrency: string) =
    let all = if List.contains groupCurrency commonCurrencies then commonCurrencies else groupCurrency :: commonCurrencies
    all |> List.map (fun c -> Html.option [ prop.value c; prop.text c ])

let private field (label: string) (input: ReactElement) : ReactElement =
    Html.div [
        prop.className "space-y-1"
        prop.children [
            Html.label [ prop.className "block text-sm font-medium text-gray-700"; prop.text label ]
            input
        ]
    ]

let private amountWithCurrencyRow (amountText: string) (currency: string) (groupCurrency: string)
    (onAmount: string -> unit) (onCurrency: string -> unit) =
    Html.div [
        prop.className "flex gap-2"
        prop.children [
            Html.div [
                prop.className "flex-1"
                prop.children [
                    Html.input [
                        prop.type' "text"
                        prop.className inputCls
                        prop.placeholder "0.00"
                        prop.value amountText
                        prop.onChange onAmount
                    ]
                ]
            ]
            Html.div [
                prop.className "w-28"
                prop.children [
                    Html.select [
                        prop.className inputCls
                        prop.value currency
                        prop.onChange (fun (v: string) -> onCurrency v)
                        prop.children (currencyOptions groupCurrency)
                    ]
                ]
            ]
        ]
    ]

let view (state: SpaceState) (rates: Map<string, decimal>) (form: SettlementForm) (dispatch: Msg -> unit) =
    let members =
        state.Members
        |> Map.toList
        |> List.map snd
        |> List.sortBy (fun m -> m.DisplayName)

    let set f = dispatch (SettlementFormSet (f form))
    let groupCur = state.Currency
    let memberOptions =
        members |> List.mapi (fun i m ->
            Html.option [ prop.value (string i); prop.text m.DisplayName ]
        )

    let isForeign1 = form.Currency <> groupCur
    let isForeign2 = form.UseSecondPayment && form.Currency2 <> groupCur
    let disabled =
        form.IsSubmitting || form.AmountText.Trim() = ""
        || (isForeign1 && form.ExchangeRateText.Trim() = "")
        || (form.UseSecondPayment && form.AmountText2.Trim() = "")
        || (isForeign2 && form.ExchangeRateText2.Trim() = "")

    Html.div [
        prop.className "max-w-lg mx-auto px-4 py-8 space-y-6"
        prop.children [
            Html.div [
                prop.className "flex items-center gap-3"
                prop.children [
                    Html.button [
                        prop.className "text-teal-600 hover:text-teal-800 text-sm font-medium"
                        prop.text "← Back"
                        prop.onClick (fun _ -> dispatch (NavigateTo SpaceOverview))
                    ]
                    Html.h1 [ prop.className "text-xl font-bold text-gray-900"; prop.text "Record Settlement" ]
                ]
            ]
            Html.div [
                prop.className "bg-white rounded-xl shadow-sm border border-gray-200 p-5 space-y-4"
                prop.children [
                    field "From"
                        (Html.select [
                            prop.className inputCls
                            prop.value (string form.FromIndex)
                            prop.onChange (fun (v: string) -> set (fun f -> { f with FromIndex = int v }))
                            prop.children memberOptions
                        ])
                    field "To"
                        (Html.select [
                            prop.className inputCls
                            prop.value (string form.ToIndex)
                            prop.onChange (fun (v: string) -> set (fun f -> { f with ToIndex = int v }))
                            prop.children memberOptions
                        ])

                    Html.div [
                        prop.className "space-y-3 border-t border-gray-100 pt-3"
                        prop.children [
                            Html.p [ prop.className "text-xs font-semibold text-gray-400 uppercase tracking-wide"; prop.text "Payment 1" ]
                            field "Amount"
                                (amountWithCurrencyRow form.AmountText form.Currency groupCur
                                    (fun v -> set (fun f -> { f with AmountText = v }))
                                    (fun v ->
                                        let rateText =
                                            FxRates.getRate v groupCur rates
                                            |> Option.map string
                                            |> Option.defaultValue ""
                                        set (fun f -> { f with Currency = v; ExchangeRateText = rateText })))
                            if isForeign1 then
                                field (sprintf "Exchange rate (%s → %s)" form.Currency groupCur)
                                    (Html.input [
                                        prop.type' "text"
                                        prop.className inputCls
                                        prop.placeholder "e.g. 0.79"
                                        prop.value form.ExchangeRateText
                                        prop.onChange (fun v -> set (fun f -> { f with ExchangeRateText = v }))
                                    ])
                        ]
                    ]

                    if form.UseSecondPayment then
                        Html.div [
                            prop.className "space-y-3 border-t border-gray-100 pt-3"
                            prop.children [
                                Html.div [
                                    prop.className "flex justify-between items-center"
                                    prop.children [
                                        Html.p [ prop.className "text-xs font-semibold text-gray-400 uppercase tracking-wide"; prop.text "Payment 2" ]
                                        Html.button [
                                            prop.className "text-xs text-red-500 hover:text-red-700"
                                            prop.text "Remove"
                                            prop.onClick (fun _ -> set (fun f -> { f with UseSecondPayment = false; AmountText2 = ""; Currency2 = groupCur; ExchangeRateText2 = "" }))
                                        ]
                                    ]
                                ]
                                field "Amount"
                                    (amountWithCurrencyRow form.AmountText2 form.Currency2 groupCur
                                        (fun v -> set (fun f -> { f with AmountText2 = v }))
                                        (fun v ->
                                            let rateText =
                                                FxRates.getRate v groupCur rates
                                                |> Option.map string
                                                |> Option.defaultValue ""
                                            set (fun f -> { f with Currency2 = v; ExchangeRateText2 = rateText })))
                                if isForeign2 then
                                    field (sprintf "Exchange rate (%s → %s)" form.Currency2 groupCur)
                                        (Html.input [
                                            prop.type' "text"
                                            prop.className inputCls
                                            prop.placeholder "e.g. 0.79"
                                            prop.value form.ExchangeRateText2
                                            prop.onChange (fun v -> set (fun f -> { f with ExchangeRateText2 = v }))
                                        ])
                            ]
                        ]

                    if not form.UseSecondPayment then
                        Html.button [
                            prop.className "w-full border border-dashed border-gray-300 hover:border-teal-400 text-gray-500 hover:text-teal-600 text-sm py-2 rounded-lg transition-colors"
                            prop.text "+ Add another currency"
                            prop.onClick (fun _ -> set (fun f -> { f with UseSecondPayment = true; Currency2 = groupCur }))
                        ]

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
                    match form.Error with
                    | Some err -> Html.p [ prop.className "text-sm text-red-600"; prop.text err ]
                    | None     -> ()
                    Html.button [
                        prop.className "w-full bg-teal-600 hover:bg-teal-700 disabled:opacity-50 text-white font-semibold py-3 rounded-xl transition-colors"
                        prop.disabled disabled
                        prop.text (if form.IsSubmitting then "Saving..." else "Record Settlement")
                        prop.onClick (fun _ -> dispatch SettlementSubmit)
                    ]
                ]
            ]
        ]
    ]
