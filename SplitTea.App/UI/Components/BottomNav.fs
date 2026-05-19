module BottomNav

open Feliz
open UITypes

let private navTab (label: string) (icon: ReactElement) (active: bool) (onClick: unit -> unit) =
    Html.button [
        prop.type' "button"
        prop.className (
            Styles.cx [
                "flex-1 flex flex-col items-center gap-1 py-2 text-xs font-medium transition-colors"
                if active then "text-teal-600" else "text-gray-400 hover:text-gray-600"
            ])
        prop.onClick (fun _ -> onClick ())
        prop.children [
            icon
            Html.span [ prop.text label ]
        ]
    ]

let bottomNav (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.className "fixed bottom-0 inset-x-0 bg-white border-t border-gray-200 z-40"
        prop.children [
            Html.div [
                prop.className "max-w-lg mx-auto flex"
                prop.children [
                    navTab "Home"      Icons.home   (model.Page = SpaceOverview) (fun () -> dispatch (NavigateTo SpaceOverview))
                    navTab "Activity"  Icons.list   (model.Page = Activity)      (fun () -> dispatch (NavigateTo Activity))
                    navTab "Analytics" Icons.chart  (model.Page = Analytics)     (fun () -> dispatch (NavigateTo Analytics))
                    navTab "Profile"   Icons.person (model.Page = Profile)       (fun () -> dispatch (NavigateTo Profile))
                ]
            ]
        ]
    ]

let spaceSwitcherSheet (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.className "fixed inset-0 z-50 flex items-end sm:items-center justify-center"
        prop.onKeyDown (fun e -> if e.key = "Escape" then dispatch SpaceSwitcherToggled)
        prop.children [
            Html.div [
                prop.className "absolute inset-0 bg-black/40"
                prop.onClick (fun _ -> dispatch SpaceSwitcherToggled)
            ]
            Html.div [
                prop.role "dialog"
                prop.custom("aria-modal", "true")
                prop.ariaLabel "Spaces"
                prop.className "relative z-10 bg-white rounded-t-2xl sm:rounded-2xl w-full sm:max-w-md p-5 space-y-3"
                prop.children [
                    Html.div [
                        prop.className "flex items-center justify-between"
                        prop.children [
                            Html.h2 [ prop.className "text-lg font-bold text-gray-900"; prop.text "Spaces" ]
                            Html.button [
                                prop.type' "button"
                                prop.ariaLabel "Close"
                                prop.className "p-1 text-gray-400 hover:text-gray-600 rounded-lg transition-colors text-lg leading-none"
                                prop.onClick (fun _ -> dispatch SpaceSwitcherToggled)
                                prop.text "✕"
                            ]
                        ]
                    ]
                    Html.div [
                        prop.className "space-y-1"
                        prop.children (
                            model.KnownSpaces |> List.map (fun s ->
                                let isActive = Some s.Id = model.ActiveSpaceId
                                Html.button [
                                    prop.type' "button"
                                    prop.className (
                                        Styles.cx [
                                            "w-full flex items-center gap-3 px-3 py-2.5 rounded-lg text-left text-sm transition-colors"
                                            if isActive then "bg-teal-50 text-teal-700 font-medium"
                                            else "text-gray-700 hover:bg-gray-50"
                                        ])
                                    prop.onClick (fun _ -> dispatch (SwitchToSpace s.Id))
                                    prop.children [
                                        Html.span [ prop.className "flex-1"; prop.text (if s.Name = "" then "(Loading...)" else s.Name) ]
                                        if isActive then Icons.check
                                    ]
                                ])
                        )
                    ]
                    Html.button [
                        prop.type' "button"
                        prop.className "w-full flex items-center gap-2 px-3 py-2.5 rounded-lg text-sm text-teal-600 hover:bg-teal-50 font-medium transition-colors"
                        prop.onClick (fun _ ->
                            dispatch SpaceSwitcherToggled
                            dispatch (NavigateTo CreateSpace))
                        prop.children [
                            Icons.plus
                            Html.span [ prop.text "Create new space" ]
                        ]
                    ]
                ]
            ]
        ]
    ]
