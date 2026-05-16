module ContextDetailPage

open Feliz
open SplitTea.Core
open UITypes

let private monthAbbr = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
let private fmtDate (d: System.DateOnly) = sprintf "%s %d, %d" monthAbbr.[d.Month - 1] d.Day d.Year
let private fmtDateShort (d: System.DateOnly) = sprintf "%s %d" monthAbbr.[d.Month - 1] d.Day

let private formatAmount (currency: string) (amount: decimal) =
    sprintf "%s %.2f" currency amount

let private memberName (state: GroupState) (id: MemberId) =
    state.Members
    |> Map.tryFind id
    |> Option.map (fun m -> m.DisplayName)
    |> Option.defaultValue "Unknown"

let private balanceRow (state: GroupState) (pos: NetPosition) =
    let name = memberName state pos.MemberId
    let currency = state.Currency
    let (color, label) =
        if pos.Amount > 0m    then "text-green-600", sprintf "gets back %s" (formatAmount currency pos.Amount)
        elif pos.Amount < 0m  then "text-red-600",   sprintf "owes %s"      (formatAmount currency -pos.Amount)
        else                       "text-gray-500",  "settled up"
    Html.div [
        prop.className "flex justify-between items-center py-2 border-b border-gray-100 last:border-0"
        prop.children [
            Html.span [ prop.className "font-medium text-gray-800"; prop.text name ]
            Html.span [ prop.className (sprintf "text-sm %s" color); prop.text label ]
        ]
    ]

let private settlementRow (state: GroupState) (s: SuggestedSettlement) =
    Html.div [
        prop.className "flex items-center gap-2 py-2 text-sm text-gray-700 border-b border-gray-100 last:border-0"
        prop.children [
            Html.span [ prop.className "font-medium"; prop.text (memberName state s.From) ]
            Html.span [ prop.className "text-gray-400"; prop.text "->" ]
            Html.span [ prop.className "font-medium"; prop.text (memberName state s.To) ]
            Html.span [ prop.className "ml-auto font-semibold text-gray-800"; prop.text (formatAmount state.Currency s.Amount) ]
        ]
    ]

let private categoryRow (currency: string) (c: CategorySpend) =
    Html.div [
        prop.className "flex justify-between items-center py-2 border-b border-gray-100 last:border-0"
        prop.children [
            Html.span [ prop.className "text-sm text-gray-700"; prop.text c.Category ]
            Html.span [ prop.className "text-sm font-semibold text-gray-800"; prop.text (formatAmount currency c.Total) ]
        ]
    ]

let private expenseRow (state: GroupState) (e: ExpenseState) =
    let amountStr =
        if e.PaidCurrency = state.Currency then
            formatAmount state.Currency e.PaidAmount
        else
            sprintf "%s %.2f (%s)" e.PaidCurrency e.PaidAmount state.Currency
    Html.div [
        prop.className "flex items-start justify-between py-2 border-b border-gray-100 last:border-0"
        prop.children [
            Html.div [
                prop.className "flex-1 min-w-0"
                prop.children [
                    Html.p [ prop.className "text-sm font-medium text-gray-800 truncate"; prop.text e.Description ]
                    Html.p [
                        prop.className "text-xs text-gray-500"
                        prop.text (sprintf "%s · paid by %s" (fmtDateShort e.Date) (memberName state e.PaidBy))
                    ]
                ]
            ]
            Html.span [ prop.className "text-sm font-semibold text-gray-800 ml-3 shrink-0"; prop.text amountStr ]
        ]
    ]

let view (state: GroupState) (contextId: ContextId) (dispatch: Msg -> unit) =
    match Map.tryFind contextId state.Contexts with
    | None ->
        Html.div [
            prop.className "max-w-lg mx-auto px-4 py-8"
            prop.children [
                Html.p [ prop.className "text-gray-500"; prop.text "Context not found." ]
            ]
        ]
    | Some ctx ->
        let positions   = Projections.computeContextNetPositions contextId state
        let settlements = Projections.computeMinimumSettlements positions
        let categories  = Projections.computeContextSpendingByCategory contextId state
        let expenses =
            state.Expenses
            |> Map.toList
            |> List.map snd
            |> List.filter (fun e -> not e.IsDeleted && e.ContextId = Some contextId)
            |> List.sortByDescending (fun e -> e.Date)

        let dateRange =
            match ctx.DateFrom, ctx.DateTo with
            | Some f, Some t -> Some (sprintf "%s – %s" (fmtDate f) (fmtDate t))
            | Some f, None   -> Some (sprintf "From %s" (fmtDate f))
            | None,   Some t -> Some (sprintf "Until %s" (fmtDate t))
            | None,   None   -> None

        Html.div [
            prop.className "max-w-lg mx-auto px-4 py-8 space-y-6"
            prop.children [
                Html.div [
                    prop.className "flex items-start gap-3"
                    prop.children [
                        Html.button [
                            prop.className "text-teal-600 hover:text-teal-800 text-sm font-medium mt-1"
                            prop.text "← Back"
                            prop.onClick (fun _ -> dispatch (NavigateTo GroupOverview))
                        ]
                        Html.div [
                            prop.children [
                                Html.h1 [ prop.className "text-xl font-bold text-gray-900"; prop.text ctx.Name ]
                                match dateRange with
                                | Some r -> Html.p [ prop.className "text-sm text-gray-500 mt-0.5"; prop.text r ]
                                | None   -> ()
                            ]
                        ]
                    ]
                ]

                Html.div [
                    prop.className "bg-white rounded-xl shadow-sm border border-gray-200 p-4"
                    prop.children [
                        Html.h2 [ prop.className "text-sm font-semibold text-gray-500 uppercase tracking-wide mb-3"; prop.text "Balances" ]
                        if List.isEmpty positions then
                            Html.p [ prop.className "text-sm text-gray-400 italic"; prop.text "No expenses yet." ]
                        else
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

                if not (List.isEmpty categories) then
                    Html.div [
                        prop.className "bg-white rounded-xl shadow-sm border border-gray-200 p-4"
                        prop.children [
                            Html.h2 [ prop.className "text-sm font-semibold text-gray-500 uppercase tracking-wide mb-3"; prop.text "By Category" ]
                            Html.div (categories |> List.map (categoryRow state.Currency))
                        ]
                    ]

                Html.div [
                    prop.className "bg-white rounded-xl shadow-sm border border-gray-200 p-4"
                    prop.children [
                        Html.h2 [ prop.className "text-sm font-semibold text-gray-500 uppercase tracking-wide mb-3"; prop.text "Expenses" ]
                        if List.isEmpty expenses then
                            Html.p [ prop.className "text-sm text-gray-400 italic"; prop.text "No expenses tagged to this context yet." ]
                        else
                            Html.div (expenses |> List.map (expenseRow state))
                    ]
                ]
            ]
        ]
