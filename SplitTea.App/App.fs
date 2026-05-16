module App

open Elmish
open Elmish.React

Program.mkProgram AppRoot.init AppRoot.update AppRoot.view
|> Program.withSubscription AppRoot.subscribe
|> Program.withReactSynchronous "root"
|> Program.run
