module DevMode

open Fable.Core
open Fable.Core.JsInterop
open System

#if DEVMODE
let private search : string = emitJsExpr () "window.location.search"

let isEnabled () =
    search.Contains("?dev") || search.Contains("&dev")

let fakeUserId = Guid.Parse "00000000-0000-0000-0000-000000000001"
let fakeUserEmail = "dev@splittea.local"
let fakeMemberName = "Local Tester"
let fakeSpaceName = "Local Test Space"
#else
let isEnabled () = false
#endif
