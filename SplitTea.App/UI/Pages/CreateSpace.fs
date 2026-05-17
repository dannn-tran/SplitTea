module CreateSpacePage

open Feliz
open UITypes

let view (hasExistingSpace: bool) (model: Model) (dispatch: Msg -> unit) =
    let form = model.CreateSpaceForm
    Html.div [
        prop.className "max-w-lg mx-auto px-4 pt-8 pb-20 space-y-6"
        prop.children [
            Html.div [
                prop.className "flex items-center gap-3"
                prop.children [
                    if hasExistingSpace then
                        Html.button [
                            prop.type' "button"
                            prop.className "text-sm text-gray-500 hover:text-gray-700 transition-colors"
                            prop.onClick (fun _ -> dispatch (NavigateTo SpaceOverview))
                            prop.text "← Back"
                        ]
                    Html.h1 [ prop.className "text-2xl font-bold text-gray-900"; prop.text "Create Space" ]
                ]
            ]

            Html.div [
                prop.className (Styles.cx [Styles.card; "p-4 space-y-4"])
                prop.children [
                    Styles.field "Space name" (
                        Html.input [
                            prop.className Styles.input
                            prop.placeholder "e.g. Trip to Japan"
                            prop.value form.SpaceNameText
                            prop.autoFocus (not hasExistingSpace)
                            prop.onChange (CreateSpaceNameSet >> dispatch)
                            prop.onKeyDown (fun e -> if e.key = "Enter" then dispatch CreateSpaceSubmit)
                        ]
                    )
                    Styles.field "Currency" (
                        Html.input [
                            prop.className Styles.input
                            prop.placeholder "e.g. SGD, USD, EUR"
                            prop.value form.CurrencyText
                            prop.onChange (CreateSpaceCurrencySet >> dispatch)
                            prop.onKeyDown (fun e -> if e.key = "Enter" then dispatch CreateSpaceSubmit)
                        ]
                    )
                    Styles.field "Your name" (
                        Html.input [
                            prop.className Styles.input
                            prop.placeholder "Your display name in this space"
                            prop.value form.MemberName
                            prop.onChange (CreateSpaceMemberSet >> dispatch)
                            prop.onKeyDown (fun e -> if e.key = "Enter" then dispatch CreateSpaceSubmit)
                        ]
                    )
                    match form.Error with
                    | Some err -> Html.p [ prop.className Styles.error; prop.text err ]
                    | None -> ()
                    Html.button [
                        prop.className Styles.btnPrimary
                        prop.disabled form.IsSubmitting
                        prop.text (if form.IsSubmitting then "Creating..." else "Create Space")
                        prop.onClick (fun _ -> dispatch CreateSpaceSubmit)
                    ]
                ]
            ]
        ]
    ]
