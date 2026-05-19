module SupabaseClient

open Fable.Core
open Fable.Core.JsInterop

[<Import("createClient", "@supabase/supabase-js")>]
let private createClientJs (url: string) (key: string) : obj = jsNative

let private supabaseUrl : string = emitJsExpr () "import.meta.env.VITE_SUPABASE_URL"
let private supabaseKey : string = emitJsExpr () "import.meta.env.VITE_SUPABASE_ANON_KEY"

let supabase : obj =
    if DevMode.isEnabled () then null
    else createClientJs supabaseUrl supabaseKey
