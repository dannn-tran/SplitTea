module SettlementFormPage

open Feliz
open SplitTea.Core
open UITypes


let private amountWithCurrencyRow (amount: Field<decimal>) (currency: string) (groupCurrency: string)
    (onAmount: Field<decimal> -> unit) (onCurrency: string -> unit) =
    Html.div [
        prop.className "flex gap-2"
        prop.children [
            Html.div [
                prop.className "flex-1"
                prop.children [
                    Html.input [
                        prop.type' "text"
                        prop.className (if amount.IsError then Styles.cx [Styles.input; "border-red-400 focus:ring-red-400"] else Styles.input)
                        prop.placeholder "0.00"
                        prop.value amount.Text
                        prop.onChange (fun v -> onAmount (Field.parseDecimal v))
                    ]
                    if amount.IsError then
                        Html.p [ prop.className Styles.error; prop.text "Invalid amount." ]
                ]
            ]
            Html.div [
                prop.className "w-28"
                prop.children [
                    Html.select [
                        prop.className Styles.input
                        prop.value currency
                        prop.onChange (fun (v: string) -> onCurrency v)
                        prop.children (Currencies.options groupCurrency)
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
        members |> List.map (fun m ->
            let (MemberId g) = m.Id
            Html.option [ prop.value (string g); prop.text m.DisplayName ]
        )

    let isForeign1 = form.Currency <> groupCur
    let isForeign2 = form.UseSecondPayment && form.Currency2 <> groupCur
    let disabled =
        form.IsSubmitting || form.Amount.Parsed = None
        || (form.UseSecondPayment && form.Amount2.Parsed = None)

    let memberIdStr (id: MemberId) = id |> fun (MemberId g) -> string g

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
                    Html.h1 [ prop.className "text-xl font-bold text-gray-900"; prop.text "Record Settlement" ]
                ]
            ]
            Html.div [
                prop.className (Styles.cx [Styles.card; "p-5 space-y-4"])
                prop.children [
                    Styles.field "From"
                        (Html.select [
                            prop.className Styles.input
                            prop.value (memberIdStr form.FromId)
                            prop.onChange (fun (v: string) -> set (fun f -> { f with FromId = MemberId (System.Guid.Parse v) }))
                            prop.children memberOptions
                        ])
                    Styles.field "To"
                        (Html.select [
                            prop.className Styles.input
                            prop.value (memberIdStr form.ToId)
                            prop.onChange (fun (v: string) -> set (fun f -> { f with ToId = MemberId (System.Guid.Parse v) }))
                            prop.children memberOptions
                        ])

                    Html.div [
                        prop.className "space-y-3 border-t border-gray-100 pt-3"
                        prop.children [
                            Html.p [ prop.className "text-xs font-semibold text-gray-400 uppercase tracking-wide"; prop.text "Payment 1" ]
                            Styles.field "Amount"
                                (amountWithCurrencyRow form.Amount form.Currency groupCur
                                    (fun v -> set (fun f -> { f with Amount = v }))
                                    (fun v ->
                                        set (fun f -> { f with Currency = v; ExchangeRate = Field.parseDecimal (Currencies.prefillRate v groupCur rates) })))
                            if isForeign1 then
                                Styles.field $"Exchange rate (%s{form.Currency} → %s{groupCur})"
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
                                            prop.onClick (fun _ -> set (fun f -> { f with UseSecondPayment = false; Amount2 = Field.emptyDecimal; Currency2 = groupCur; ExchangeRate2 = Field.emptyDecimal }))
                                        ]
                                    ]
                                ]
                                Styles.field "Amount"
                                    (amountWithCurrencyRow form.Amount2 form.Currency2 groupCur
                                        (fun v -> set (fun f -> { f with Amount2 = v }))
                                        (fun v ->
                                            set (fun f -> { f with Currency2 = v; ExchangeRate2 = Field.parseDecimal (Currencies.prefillRate v groupCur rates) })))
                                if isForeign2 then
                                    Styles.field $"Exchange rate (%s{form.Currency2} → %s{groupCur})"
                                        (Html.div [ prop.className "space-y-1"; prop.children [
                                            Html.input [
                                                prop.type' "text"
                                                prop.className (if form.ExchangeRate2.IsError then Styles.cx [Styles.input; "border-red-400 focus:ring-red-400"] else Styles.input)
                                                prop.placeholder "e.g. 0.79"
                                                prop.value form.ExchangeRate2.Text
                                                prop.onChange (fun v -> set (fun f -> { f with ExchangeRate2 = Field.parseDecimal v }))
                                            ]
                                            if form.ExchangeRate2.IsError then
                                                Html.p [ prop.className Styles.error; prop.text "Invalid exchange rate." ]
                                        ]])
                            ]
                        ]

                    if not form.UseSecondPayment then
                        Html.button [
                            prop.className "w-full border border-dashed border-gray-300 hover:border-teal-400 text-gray-500 hover:text-teal-600 text-sm py-2 rounded-lg transition-colors"
                            prop.text "+ Add another currency"
                            prop.onClick (fun _ -> set (fun f -> { f with UseSecondPayment = true; Currency2 = groupCur }))
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
                        prop.text (if form.IsSubmitting then "Saving..." else "Record Settlement")
                        prop.onClick (fun _ -> dispatch SettlementSubmit)
                    ]
                ]
            ]
        ]
    ]
