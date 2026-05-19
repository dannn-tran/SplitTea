module SignInPage

open Feliz
open UITypes

let view (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.className "min-h-screen bg-gray-50 flex items-center justify-center px-4"
        prop.children [
            Html.div [
                prop.className "w-full max-w-sm space-y-6"
                prop.children [
                    Html.h1 [ prop.className "text-3xl font-bold text-center text-teal-700"; prop.text "SplitTea" ]
                    Html.div [
                        prop.className "bg-white rounded-xl shadow-sm border border-gray-200 p-6 space-y-4"
                        prop.children [
                            Html.label [
                                prop.className "block text-sm font-medium text-gray-700"
                                prop.text "Email"
                                prop.htmlFor "email"
                            ]
                            Html.input [
                                prop.id "email"
                                prop.type' "email"
                                prop.className "w-full border border-gray-300 rounded-lg px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-teal-500"
                                prop.placeholder "you@example.com"
                                prop.value model.SignInEmail
                                prop.onChange (SignInEmailSet >> dispatch)
                                prop.onKeyDown (fun e -> if e.key = "Enter" then dispatch SignInSubmit)
                            ]
                            match model.SignInError with
                            | Some msg -> Html.p [ prop.className "text-sm text-gray-600"; prop.text msg ]
                            | None     -> ()
                            Html.button [
                                prop.className "w-full bg-teal-600 hover:bg-teal-700 disabled:opacity-50 text-white font-semibold py-2 rounded-lg transition-colors"
                                prop.disabled model.IsAuthLoading
                                prop.text (if model.IsAuthLoading then "Sending..." else "Send Magic Link")
                                prop.onClick (fun _ -> dispatch SignInSubmit)
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

#if DEVMODE
let devBootstrapView (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.className "min-h-screen bg-gray-50 flex items-center justify-center px-4"
        prop.children [
            Html.div [
                prop.className "w-full max-w-sm space-y-6"
                prop.children [
                    Html.h1 [ prop.className "text-3xl font-bold text-center text-teal-700"; prop.text "SplitTea" ]
                    Html.div [
                        prop.className "bg-white rounded-xl shadow-sm border border-gray-200 p-6 space-y-4"
                        prop.children [
                            Html.p [
                                prop.className "text-sm text-gray-600"
                                prop.text "Dev mode is active. Create a local-only test space stored in IndexedDB."
                            ]
                            Html.button [
                                prop.className "w-full bg-teal-600 hover:bg-teal-700 disabled:opacity-50 text-white font-semibold py-2 rounded-lg transition-colors"
                                prop.disabled model.IsAuthLoading
                                prop.text (if model.IsAuthLoading then "Creating..." else "Create Local Test Space")
                                prop.onClick (fun _ -> dispatch CreateDevSpace)
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
#endif
