module App

open Elmish
open Elmish.React
open Feliz

type Model = unit
type Msg = | NoOp

let init () : Model * Cmd<Msg> = (), Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | NoOp -> model, Cmd.none

let view (_model: Model) (_dispatch: Msg -> unit) =
    Html.div [
        prop.className "min-h-screen bg-gray-50 flex items-center justify-center"
        prop.children [
            Html.h1 [
                prop.className "text-3xl font-bold text-gray-900"
                prop.text "SplitTea"
            ]
        ]
    ]

Program.mkProgram init update view
|> Program.withReactSynchronous "root"
|> Program.run
