module Currencies

open Feliz

let common = [ "AUD"; "CAD"; "CHF"; "CNY"; "EUR"; "GBP"; "HKD"; "JPY"; "MYR"; "NZD"; "SGD"; "USD" ]

let options (groupCurrency: string) =
    let all = if List.contains groupCurrency common then common else groupCurrency :: common
    all |> List.map (fun c -> Html.option [ prop.value c; prop.text c ])

let prefillRate (currency: string) (groupCurrency: string) (rates: Map<string, decimal>) : string =
    FxRates.getRate currency groupCurrency rates
    |> Option.map string
    |> Option.defaultValue ""
