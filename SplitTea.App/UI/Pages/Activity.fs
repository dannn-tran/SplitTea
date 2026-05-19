module ActivityPage

open Feliz
open SplitTea.Core

[<RequireQualifiedAccess>]
type private Item =
    | Expense    of ExpenseState
    | Settlement of SettlementRecordedPayload

let private memberName (state: SpaceState) (id: MemberId) =
    state.Members
    |> Map.tryFind id
    |> Option.map (fun m -> m.DisplayName)
    |> Option.defaultValue "Unknown"

let private formatDate (d: System.DateOnly) =
    $"%04d{d.Year}-%02d{d.Month}-%02d{d.Day}"

let private formatAmount (currency: string) (amount: decimal) =
    $"%s{currency} %.2f{amount}"

let private expenseRow (state: SpaceState) (e: ExpenseState) =
    let payer = memberName state e.PaidBy
    let amtStr =
        if e.PaidCurrency = state.Currency then formatAmount state.Currency e.PaidAmount
        else sprintf "%s (→ %s)" (formatAmount e.PaidCurrency e.PaidAmount) state.Currency
    Html.div [
        prop.className "py-3 border-b border-gray-100 last:border-0 space-y-0.5"
        prop.children [
            Html.div [
                prop.className "flex justify-between items-start gap-2"
                prop.children [
                    Html.div [
                        prop.className "flex-1 min-w-0"
                        prop.children [
                            Html.p [ prop.className "text-sm font-medium text-gray-900 truncate"; prop.text e.Description ]
                            Html.p [ prop.className "text-xs text-gray-500"; prop.text $"{payer} paid {amtStr}" ]
                        ]
                    ]
                    Html.div [
                        prop.className "text-right shrink-0"
                        prop.children [
                            Html.p [ prop.className "text-xs text-gray-400"; prop.text (formatDate e.Date) ]
                            match e.Category with
                            | Some cat ->
                                Html.p [ prop.className "text-xs text-teal-600 font-medium"; prop.text cat ]
                            | None -> Html.none
                        ]
                    ]
                ]
            ]
        ]
    ]

let private settlementRow (state: SpaceState) (s: SettlementRecordedPayload) =
    let fromName = memberName state s.From
    let toName   = memberName state s.To
    let amtStr   =
        s.Payments
        |> List.map (fun leg -> formatAmount leg.Currency leg.Amount)
        |> String.concat " + "
    Html.div [
        prop.className "py-3 border-b border-gray-100 last:border-0"
        prop.children [
            Html.div [
                prop.className "flex justify-between items-start gap-2"
                prop.children [
                    Html.div [
                        prop.className "flex-1 min-w-0"
                        prop.children [
                            Html.p [
                                prop.className "text-sm font-medium text-gray-900"
                                prop.text $"{fromName} → {toName}"
                            ]
                            Html.p [ prop.className "text-xs text-gray-500"; prop.text $"Settlement · {amtStr}" ]
                        ]
                    ]
                    Html.p [ prop.className "text-xs text-gray-400 shrink-0"; prop.text (formatDate s.Date) ]
                ]
            ]
        ]
    ]

let view (state: SpaceState) =
    let expenses =
        state.Expenses
        |> Map.toList
        |> List.map snd
        |> List.filter (fun e -> not e.IsDeleted)
        |> List.map Item.Expense

    let settlements =
        state.Settlements
        |> List.map Item.Settlement

    let allItems =
        expenses @ settlements
        |> List.sortByDescending (function
            | Item.Expense e    -> e.Date
            | Item.Settlement s -> s.Date)

    Html.div [
        prop.className "min-h-screen bg-gray-50 pb-20"
        prop.children [
            Html.div [
                prop.className "sticky top-0 bg-white border-b border-gray-200 z-10"
                prop.children [
                    Html.div [
                        prop.className "max-w-lg mx-auto px-4 py-4"
                        prop.children [
                            Html.h1 [ prop.className "text-lg font-bold text-gray-900"; prop.text "Activity" ]
                        ]
                    ]
                ]
            ]
            Html.div [
                prop.className "max-w-lg mx-auto px-4 py-2"
                prop.children [
                    if allItems.IsEmpty then
                        Html.p [ prop.className "text-center text-gray-400 text-sm py-12"; prop.text "No activity yet." ]
                    else
                        Html.div [
                            prop.className "bg-white rounded-xl shadow-sm divide-y divide-gray-100 px-4"
                            prop.children (
                                allItems |> List.mapi (fun i item ->
                                    match item with
                                    | Item.Expense e    -> expenseRow state e
                                    | Item.Settlement s -> settlementRow state s
                                )
                            )
                        ]
                ]
            ]
        ]
    ]
