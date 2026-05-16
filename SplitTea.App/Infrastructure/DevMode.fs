module DevMode

open Fable.Core
open Fable.Core.JsInterop

#if DEVMODE
let private search : string = emitJsExpr () "window.location.search"

let isEnabled () =
    search.Contains("?dev") || search.Contains("&dev")
#else
let isEnabled () = false
#endif
