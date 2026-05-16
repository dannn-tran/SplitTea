module ContextFormPage

open Feliz
open SplitTea.Core
open UITypes

let private inputCls = "w-full border border-gray-300 rounded-lg px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-teal-500"

let private monthAbbr = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
let private monthFull = [| "January"; "February"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December" |]
let private dateStr (d: System.DateOnly) = sprintf "%04d-%02d-%02d" d.Year d.Month d.Day

let private isoWeekNum (d: System.DateOnly) : int =
    let dow = match d.DayOfWeek with System.DayOfWeek.Sunday -> 7 | x -> int x
    let week = (d.DayOfYear - dow + 10) / 7
    if week < 1 then
        let prevDec31 = System.DateOnly(d.Year - 1, 12, 31)
        let prevDec31dow = match prevDec31.DayOfWeek with System.DayOfWeek.Sunday -> 7 | x -> int x
        if prevDec31dow >= 4 then 53 else 52
    elif week > 52 then
        let dec31 = System.DateOnly(d.Year, 12, 31)
        let dec31dow = match dec31.DayOfWeek with System.DayOfWeek.Sunday -> 7 | x -> int x
        if dec31dow >= 4 then week else 1
    else
        week

let private field (label: string) (input: ReactElement) =
    Html.div [
        prop.className "space-y-1"
        prop.children [
            Html.label [ prop.className "block text-sm font-medium text-gray-700"; prop.text label ]
            input
        ]
    ]

let private templateChip (label: string) (t: ContextTemplate) (current: ContextTemplate) (onSelect: ContextTemplate -> unit) =
    let active = t = current
    Html.button [
        prop.type' "button"
        prop.className (sprintf "px-3 py-1.5 rounded-lg text-sm font-medium border transition-colors %s"
            (if active then "bg-teal-600 text-white border-teal-600"
             else "bg-white text-gray-600 border-gray-300 hover:border-teal-400"))
        prop.text label
        prop.onClick (fun _ -> onSelect t)
    ]

// Template presets auto-fill name + dates for Monthly/Weekly.
let private applyPreset (t: ContextTemplate) (form: ContextForm) : ContextForm =
    match t with
    | Monthly ->
        let now = System.DateTime.Now
        let name = sprintf "%s %d" monthFull.[now.Month - 1] now.Year
        let dateFrom = sprintf "%04d-%02d-01" now.Year now.Month
        let daysInMonth = System.DateTime.DaysInMonth(now.Year, now.Month)
        let dateTo = sprintf "%04d-%02d-%02d" now.Year now.Month daysInMonth
        { form with Template = Monthly; Name = name; DateFromText = dateFrom; DateToText = dateTo }
    | Weekly ->
        let now = System.DateTime.Now
        let today = System.DateOnly(now.Year, now.Month, now.Day)
        let dow = match today.DayOfWeek with System.DayOfWeek.Sunday -> 7 | x -> int x
        let monday = today.AddDays(1 - dow)
        let sunday = monday.AddDays(6)
        let week = isoWeekNum monday
        let name = sprintf "Week %d · %s %d" week monthAbbr.[monday.Month - 1] monday.Day
        { form with Template = Weekly; Name = name
                    DateFromText = dateStr monday
                    DateToText   = dateStr sunday }
    | t' -> { form with Template = t' }

let view (_state: GroupState) (form: ContextForm) (dispatch: Msg -> unit) =
    let set f = dispatch (ContextFormSet (f form))

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
                    Html.h1 [ prop.className "text-xl font-bold text-gray-900"; prop.text "New Context" ]
                ]
            ]

            Html.div [
                prop.className "bg-white rounded-xl shadow-sm border border-gray-200 p-5 space-y-4"
                prop.children [
                    Html.div [
                        prop.className "space-y-2"
                        prop.children [
                            Html.label [ prop.className "block text-sm font-medium text-gray-700"; prop.text "Type" ]
                            Html.div [
                                prop.className "flex gap-2 flex-wrap"
                                prop.children [
                                    templateChip "Trip"    Trip    form.Template (fun t -> set (applyPreset t))
                                    templateChip "Monthly" Monthly form.Template (fun t -> set (applyPreset t))
                                    templateChip "Weekly"  Weekly  form.Template (fun t -> set (applyPreset t))
                                    templateChip "Custom"  Custom  form.Template (fun t -> set (applyPreset t))
                                ]
                            ]
                        ]
                    ]

                    field "Name"
                        (Html.input [
                            prop.type' "text"
                            prop.className inputCls
                            prop.placeholder "e.g. Tokyo 2025, January 2025"
                            prop.value form.Name
                            prop.onChange (fun v -> set (fun f -> { f with Name = v }))
                        ])

                    field "Start date (optional)"
                        (Html.input [
                            prop.type' "date"
                            prop.className inputCls
                            prop.value form.DateFromText
                            prop.onChange (fun v -> set (fun f -> { f with DateFromText = v }))
                        ])

                    field "End date (optional)"
                        (Html.input [
                            prop.type' "date"
                            prop.className inputCls
                            prop.value form.DateToText
                            prop.onChange (fun v -> set (fun f -> { f with DateToText = v }))
                        ])

                    match form.Error with
                    | Some err -> Html.p [ prop.className "text-sm text-red-600"; prop.text err ]
                    | None     -> ()

                    Html.button [
                        prop.className "w-full bg-teal-600 hover:bg-teal-700 disabled:opacity-50 text-white font-semibold py-3 rounded-xl transition-colors"
                        prop.disabled (form.IsSubmitting || form.Name.Trim() = "")
                        prop.text (if form.IsSubmitting then "Creating..." else "Create Context")
                        prop.onClick (fun _ -> dispatch ContextSubmit)
                    ]
                ]
            ]
        ]
    ]
