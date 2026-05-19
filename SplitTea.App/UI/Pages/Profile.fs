module ProfilePage

open Feliz
open UITypes

let view (displayName: string) (email: string option) (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.className "max-w-lg mx-auto px-4 pt-8 pb-20 space-y-6"
        prop.children [
            Html.h1 [ prop.className "text-2xl font-bold text-gray-900"; prop.text "Profile" ]

            Html.div [
                prop.className (Styles.cx [Styles.card; "p-4 space-y-4"])
                prop.children [
                    Html.div [
                        prop.className "space-y-1"
                        prop.children [
                            Html.label [ prop.className Styles.label; prop.text "Your name" ]
                            if model.IsEditingProfileName then
                                Html.div [
                                    prop.className "flex gap-2 items-center"
                                    prop.children [
                                        Html.input [
                                            prop.className Styles.inputFlex
                                            prop.value model.ProfileNameText
                                            prop.autoFocus true
                                            prop.onChange (ProfileNameTextSet >> dispatch)
                                            prop.onKeyDown (fun e ->
                                                if e.key = "Enter" then dispatch SaveProfileRename)
                                        ]
                                        Html.button [
                                            prop.type' "button"
                                            prop.className Styles.btnIconSmPrimary
                                            prop.title "Save"
                                            prop.ariaLabel "Save name"
                                            prop.onClick (fun _ -> dispatch SaveProfileRename)
                                            prop.children [ Icons.check ]
                                        ]
                                    ]
                                ]
                            else
                                Html.div [
                                    prop.className "flex items-center gap-2"
                                    prop.children [
                                        Html.span [ prop.className "flex-1 text-sm text-gray-800"; prop.text displayName ]
                                        Html.button [
                                            prop.type' "button"
                                            prop.className Styles.btnIconSmPrimary
                                            prop.title "Edit name"
                                            prop.ariaLabel "Edit name"
                                            prop.onClick (fun _ -> dispatch StartProfileRename)
                                            prop.children [ Icons.pencil ]
                                        ]
                                    ]
                                ]
                            match model.ProfileNameError with
                            | Some err -> Html.p [ prop.className Styles.error; prop.text err ]
                            | None -> ()
                        ]
                    ]
                    match email with
                    | Some e ->
                        Html.div [
                            prop.className "space-y-1"
                            prop.children [
                                Html.label [ prop.className Styles.label; prop.text "Email" ]
                                Html.p [ prop.className "text-sm text-gray-700"; prop.text e ]
                            ]
                        ]
                    | None -> ()
                ]
            ]

            if model.InstallPromptAvailable then
                Html.div [
                    prop.className (Styles.cx [Styles.card; "p-4"])
                    prop.children [
                        Html.div [
                            prop.className "flex items-start justify-between gap-3"
                            prop.children [
                                Html.div [
                                    prop.className "space-y-1"
                                    prop.children [
                                        Html.p [ prop.className "text-sm font-semibold text-gray-900"; prop.text "Install SplitTea" ]
                                        Html.p [ prop.className "text-xs text-gray-500"; prop.text "Add to your home screen for faster access." ]
                                    ]
                                ]
                                Html.button [
                                    prop.type' "button"
                                    prop.className "text-gray-400 hover:text-gray-600 text-lg leading-none"
                                    prop.text "×"
                                    prop.onClick (fun _ -> dispatch DismissInstallPrompt)
                                ]
                            ]
                        ]
                        Html.button [
                            prop.className (Styles.cx [Styles.btnPrimary; "mt-3 text-sm py-2"])
                            prop.text "Install"
                            prop.onClick (fun _ -> dispatch InstallApp)
                        ]
                    ]
                ]

            Html.button [
                prop.className "w-full text-sm text-red-600 hover:text-red-800 font-medium py-2 transition-colors"
                prop.text "Sign out"
                prop.onClick (fun _ -> dispatch SignOut)
            ]
        ]
    ]
