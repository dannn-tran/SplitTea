module FxRates

open Fable.Core
open Fable.Core.JsInterop

// ─── JS helpers ───────────────────────────────────────────────────────────────

let private localStorage : obj = emitJsExpr () "localStorage"

let private objKeys (o: obj) : string list =
    emitJsExpr o "Object.keys($0)" |> unbox<string[]> |> Array.toList

// Native browser fetch returning the parsed JSON body.
let private fetchJson (url: string) : Async<obj> =
    async {
        let! resp = emitJsExpr url "fetch($0)" |> Async.AwaitPromise
        return! emitJsExpr resp "$0.json()" |> Async.AwaitPromise
    }

// ─── Cache ────────────────────────────────────────────────────────────────────

[<Literal>]
let private cacheKey = "fxRatesCache"

[<Literal>]
let private cacheTtlMs = 86_400_000.0  // 24 hours

let private tryReadCache (baseCurrency: string) : Map<string, decimal> option =
    try
        let raw : obj = localStorage?getItem(cacheKey)
        if isNull raw then None
        else
            let json : obj = JS.JSON.parse (string raw)
            if string json?``base`` <> baseCurrency then None
            else
                let age = emitJsExpr () "Date.now()" - float json?fetchedAt
                if age > cacheTtlMs then None
                else
                    let ratesObj : obj = json?rates
                    objKeys ratesObj
                    |> List.map (fun k -> k, decimal (float ratesObj?(k)))
                    |> Map.ofList
                    |> Some
    with _ -> None

let private writeCache (baseCurrency: string) (rates: Map<string, decimal>) =
    try
        let ratesObj =
            rates
            |> Map.toList
            |> List.map (fun (k, v) -> k, box (float v))
            |> createObj
        let payload = createObj [
            "base"      ==> baseCurrency
            "rates"     ==> ratesObj
            "fetchedAt" ==> emitJsExpr () "Date.now()"
        ]
        localStorage?setItem(cacheKey, JS.JSON.stringify payload) |> ignore
    with _ -> ()

// ─── Fetch from edge function ─────────────────────────────────────────────────

let private edgeFnUrl : string = emitJsExpr () "import.meta.env.VITE_SUPABASE_URL"

let private fetchRates (baseCurrency: string) : Async<Map<string, decimal>> =
    async {
        let url = sprintf "%s/functions/v1/fx-rates?base=%s" edgeFnUrl baseCurrency
        let! json = fetchJson url
        let ratesObj : obj = json?rates
        return
            objKeys ratesObj
            |> List.map (fun k -> k, decimal (float ratesObj?(k)))
            |> Map.ofList
    }

// ─── Public API ───────────────────────────────────────────────────────────────

/// Returns rates (others relative to baseCurrency). Reads cache first; fetches if stale/missing.
let getRates (baseCurrency: string) : Async<Map<string, decimal>> =
    async {
        match tryReadCache baseCurrency with
        | Some rates -> return rates
        | None ->
            let! rates = fetchRates baseCurrency
            writeCache baseCurrency rates
            return rates
    }

/// Look up baseCurrency → targetCurrency rate from a rates map.
/// Returns None if rate unavailable (e.g. offline with empty map).
let getRate (baseCurrency: string) (targetCurrency: string) (rates: Map<string, decimal>) : decimal option =
    if baseCurrency = targetCurrency then Some 1m
    else Map.tryFind targetCurrency rates
