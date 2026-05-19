module Overlays

open Feliz
open SplitTea.Core
open UITypes

let conflictBanner (model: Model) (dispatch: Msg -> unit) (isSpaceTab: bool) =
    if not isSpaceTab || model.Conflicts.IsEmpty then Html.none
    else
        Html.div [
            prop.className "fixed bottom-14 inset-x-0 z-30"
            prop.children [
                Html.div [
                    prop.className "max-w-lg mx-auto px-3"
                    prop.children [
                        Html.div [
                            prop.className "bg-amber-50 border border-amber-200 rounded-xl shadow-md overflow-hidden"
                            prop.children [
                                Html.div [
                                    prop.className "flex items-center px-4 py-2 bg-amber-100"
                                    prop.children [
                                        Html.span [
                                            prop.className "text-amber-800 font-semibold text-sm"
                                            prop.text (
                                                let n = model.Conflicts.Length
                                                if n = 1 then "1 sync conflict" else sprintf "%d sync conflicts" n)
                                        ]
                                    ]
                                ]
                                Html.div [
                                    prop.className "divide-y divide-amber-200 max-h-28 overflow-y-auto"
                                    prop.children (
                                        model.Conflicts |> List.map (fun c ->
                                            Html.div [
                                                prop.key (let (EventId g) = c.EventId in string g)
                                                prop.className "flex items-center justify-between px-4 py-2 gap-2"
                                                prop.children [
                                                    Html.div [
                                                        prop.className "flex-1 min-w-0"
                                                        prop.children [
                                                            Html.p [ prop.className "text-xs font-medium text-gray-800 truncate"; prop.text c.Description ]
                                                            Html.p [ prop.className "text-xs text-amber-700 truncate"; prop.text c.Reason ]
                                                        ]
                                                    ]
                                                    Html.button [
                                                        prop.type' "button"
                                                        prop.className "shrink-0 text-gray-400 hover:text-gray-600 text-base leading-none px-1"
                                                        prop.ariaLabel "Dismiss conflict"
                                                        prop.onClick (fun _ -> dispatch (DismissConflict c.EventId))
                                                        prop.children [ Html.span [ prop.text "×" ] ]
                                                    ]
                                                ]
                                            ])
                                    )
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]

let confirmModal (model: Model) (dispatch: Msg -> unit) =
    match model.ConfirmDialog with
    | None -> Html.none
    | Some req ->
        Html.div [
            prop.className "fixed inset-0 z-50 flex items-center justify-center"
            prop.onKeyDown (fun e -> if e.key = "Escape" then dispatch (ConfirmResolved false))
            prop.children [
                Html.div [
                    prop.className "absolute inset-0 bg-black/40"
                    prop.onClick (fun _ -> dispatch (ConfirmResolved false))
                ]
                Html.div [
                    prop.role "dialog"
                    prop.custom("aria-modal", "true")
                    prop.ariaLabel "Confirm action"
                    prop.className "relative bg-white rounded-2xl shadow-xl p-6 mx-4 max-w-sm w-full space-y-4"
                    prop.children [
                        Html.p [
                            prop.className "text-sm text-gray-700"
                            prop.text req.Message
                        ]
                        Html.div [
                            prop.className "flex gap-3"
                            prop.children [
                                Html.button [
                                    prop.type' "button"
                                    prop.className Styles.btnBarSecondary
                                    prop.text "Cancel"
                                    prop.autoFocus true
                                    prop.onClick (fun _ -> dispatch (ConfirmResolved false))
                                ]
                                Html.button [
                                    prop.type' "button"
                                    prop.className "flex-1 bg-red-600 hover:bg-red-700 text-white font-semibold py-3 rounded-xl transition-colors"
                                    prop.text "Confirm"
                                    prop.onClick (fun _ -> dispatch (ConfirmResolved true))
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]

#if DEVMODE
let devActorBadge (model: Model) (dispatch: Msg -> unit) =
    let members =
        model.SpaceState.Members
        |> Map.toList
        |> List.sortBy (fun (_, m) -> m.DisplayName)
    Html.div [
        prop.className "fixed bottom-24 right-4 z-50 bg-gray-800 text-white text-xs rounded-lg px-3 py-2 shadow-lg flex items-center gap-2"
        prop.children [
            Html.span [ prop.className "text-gray-400"; prop.text "Acting as" ]
            Html.select [
                prop.className "bg-gray-700 text-white text-xs rounded px-1.5 py-0.5 focus:outline-none"
                prop.value (
                    model.DevActorId
                    |> Option.map (fun (MemberId g) -> string g)
                    |> Option.defaultValue "")
                prop.onChange (fun (v: string) ->
                    try dispatch (DevActorSet (MemberId (System.Guid.Parse v)))
                    with _ -> ())
                prop.children (
                    members |> List.map (fun (MemberId g, m) ->
                        Html.option [ prop.value (string g); prop.text m.DisplayName ]))
            ]
            Html.span [ prop.className "text-gray-600"; prop.text "|" ]
            Html.button [
                prop.type' "button"
                prop.className "text-red-400 hover:text-red-300 transition-colors"
                prop.title "Clear all data and restart"
                prop.text "↺ Reset"
                prop.onClick (fun _ -> dispatch DevReset)
            ]
        ]
    ]
#endif
