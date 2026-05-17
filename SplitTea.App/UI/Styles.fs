module Styles

open Feliz

// Combine class strings, filtering blanks so conditional classes can safely be ""
let cx (classes: string list) =
    classes |> List.filter (fun s -> s.Trim() <> "") |> String.concat " "

// Base input — no width prefix; compose with w-full / flex-1 as needed
let inputBase = "border border-gray-300 rounded-lg px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-teal-500"
let input     = "w-full " + inputBase
let inputFlex = "flex-1 " + inputBase

// Text
let label          = "block text-sm font-medium text-gray-700"
let error          = "text-sm text-red-600"
let sectionHeading = "text-sm font-semibold text-gray-500 uppercase tracking-wide"
let linkPrimary    = "text-xs text-teal-600 hover:text-teal-800 font-medium transition-colors"

// Buttons — full-width form submit
let btnPrimary = "w-full bg-teal-600 hover:bg-teal-700 disabled:opacity-50 text-white font-semibold py-3 rounded-xl transition-colors"

// Buttons — bottom action bar (flex-1, no disabled style)
let btnBarPrimary   = "flex-1 bg-teal-600 hover:bg-teal-700 text-white font-semibold py-3 rounded-xl transition-colors"
let btnBarSecondary = "flex-1 bg-gray-100 hover:bg-gray-200 text-gray-700 font-semibold py-3 rounded-xl transition-colors"

// Buttons — navigation & icon
let btnText          = "text-teal-600 hover:text-teal-800 text-sm font-medium"
let btnIcon          = "p-1.5 text-gray-400 hover:text-gray-600 rounded-lg hover:bg-gray-100 transition-colors"
let btnIconSmPrimary = "p-1 text-gray-400 hover:text-teal-600 transition-colors"
let btnIconSmDanger  = "p-1 text-gray-400 hover:text-red-500 transition-colors"

// Buttons — compact inline (within forms / modals)
let btnInlinePrimary   = "bg-teal-600 hover:bg-teal-700 disabled:opacity-50 text-white text-sm font-semibold px-3 py-2 rounded-lg transition-colors"
let btnInlineSecondary = "text-sm text-gray-500 hover:text-gray-700"

// Cards
let card = "bg-white rounded-xl shadow-sm border border-gray-200"

// Form field wrapper: label above, content below
let field (labelText: string) (content: ReactElement) : ReactElement =
    Html.div [
        prop.className "space-y-1"
        prop.children [
            Html.label [ prop.className label; prop.text labelText ]
            content
        ]
    ]
