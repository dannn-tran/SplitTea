module AnalyticsPage

open Feliz
open SplitTea.Core

let private formatAmount (currency: string) (amount: decimal) =
    $"%s{currency} %.2f{amount}"

let private spendRow (label: string) (currency: string) (amount: decimal) =
    Html.div [
        prop.className "flex justify-between items-center py-2 border-b border-gray-100 last:border-0"
        prop.children [
            Html.span [ prop.className "text-sm text-gray-800"; prop.text label ]
            Html.span [ prop.className "text-sm font-semibold text-gray-900"; prop.text (formatAmount currency amount) ]
        ]
    ]

let view (state: SpaceState) =
    let categorySpend = Projections.computeSpendingByCategory state
    let payerSpend    = Projections.computeSpendingByPayer state

    Html.div [
        prop.className "max-w-lg mx-auto px-4 pt-8 pb-20 space-y-6"
        prop.children [
            Html.h1 [ prop.className "text-2xl font-bold text-gray-900"; prop.text "Analytics" ]

            Html.div [
                prop.className (Styles.cx [Styles.card; "p-4"])
                prop.children [
                    Html.h2 [ prop.className (Styles.cx [Styles.sectionHeading; "mb-3"]); prop.text "By category" ]
                    if List.isEmpty categorySpend then
                        Html.p [ prop.className "text-sm text-gray-400 italic"; prop.text "No categorized expenses yet." ]
                    else
                        Html.div (categorySpend |> List.map (fun row -> spendRow row.Category state.Currency row.Total))
                ]
            ]

            Html.div [
                prop.className (Styles.cx [Styles.card; "p-4"])
                prop.children [
                    Html.h2 [ prop.className (Styles.cx [Styles.sectionHeading; "mb-3"]); prop.text "By member" ]
                    if List.isEmpty payerSpend then
                        Html.p [ prop.className "text-sm text-gray-400 italic"; prop.text "No expenses yet." ]
                    else
                        Html.div (
                            payerSpend |> List.map (fun row ->
                                let name =
                                    state.Members
                                    |> Map.tryFind row.MemberId
                                    |> Option.map _.DisplayName
                                    |> Option.defaultValue "Unknown"
                                spendRow name state.Currency row.Total)
                        )
                ]
            ]
        ]
    ]
