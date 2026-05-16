module GroupPage

open Feliz
open SplitTea.Core

let private memberName (state: GroupState) (id: MemberId) =
    state.Members
    |> Map.tryFind id
    |> Option.map (fun m -> m.DisplayName)
    |> Option.defaultValue "Unknown"

let private formatAmount (currency: string) (amount: decimal) =
    sprintf "%s %.2f" currency amount

let private balanceRow (state: GroupState) (pos: NetPosition) =
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

let private settlementRow (state: GroupState) (s: SuggestedSettlement) =
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

let private templateLabel (t: ContextTemplate) =
    match t with
    | Trip    -> "Trip"
    | Monthly -> "Monthly"
    | Weekly  -> "Weekly"
    | Custom  -> "Custom"

let private contextRow (ctx: ContextState) (dispatch: UITypes.Msg -> unit) =
    Html.button [
        prop.className "w-full flex items-center justify-between py-2 border-b border-gray-100 last:border-0 hover:bg-gray-50 px-1 rounded text-left"
        prop.onClick (fun _ -> dispatch (UITypes.OpenContext ctx.ContextId))
        prop.children [
            Html.span [ prop.className "font-medium text-gray-800 text-sm"; prop.text ctx.Name ]
            Html.span [ prop.className "text-xs text-teal-600 font-medium"; prop.text (templateLabel ctx.Template) ]
        ]
    ]

let view (state: GroupState) (dispatch: UITypes.Msg -> unit) =
    let positions    = Projections.computeNetPositions state
    let settlements  = Projections.computeMinimumSettlements positions
    let groupName    = if state.Name = "" then "Group" else state.Name
    let contexts     = state.Contexts |> Map.toList |> List.map snd |> List.sortBy (fun c -> c.Name)

    Html.div [
        prop.className "max-w-lg mx-auto px-4 py-8 space-y-6"
        prop.children [
            Html.h1 [ prop.className "text-2xl font-bold text-gray-900"; prop.text groupName ]

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
                prop.className "bg-white rounded-xl shadow-sm border border-gray-200 p-4"
                prop.children [
                    Html.div [
                        prop.className "flex items-center justify-between mb-3"
                        prop.children [
                            Html.h2 [ prop.className "text-sm font-semibold text-gray-500 uppercase tracking-wide"; prop.text "Contexts" ]
                            Html.button [
                                prop.className "text-sm text-teal-600 hover:text-teal-800 font-medium"
                                prop.text "+ New"
                                prop.onClick (fun _ -> dispatch UITypes.CreateContextClick)
                            ]
                        ]
                    ]
                    if List.isEmpty contexts then
                        Html.p [ prop.className "text-sm text-gray-400 italic"; prop.text "No contexts yet. Create a trip or monthly view." ]
                    else
                        Html.div (contexts |> List.map (fun c -> contextRow c dispatch))
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
