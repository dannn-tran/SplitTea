module AuthUpdate

open Elmish
open UITypes

let handle (event: Auth.AuthEvent) (model: Model) : Model * Cmd<Msg> =
    match event with
    | Auth.SignedIn user ->
        let page =
            match model.Page with
            | Loading ->
                if model.ActiveSpaceId.IsSome then Loading
#if DEVMODE
                elif DevMode.isEnabled () then DevBootstrap
#endif
                else CreateSpace
            | p -> p
        { model with Auth = Some user; IsAuthLoading = false; Page = page }, Cmd.none
    | Auth.SignedOut ->
        let page =
#if DEVMODE
            if DevMode.isEnabled () then DevBootstrap else SignIn
#else
            SignIn
#endif
        { model with Auth = None; IsAuthLoading = false; Page = page }, Cmd.none

let handleSignInEmailSet (email: string) (model: Model) : Model * Cmd<Msg> =
    { model with SignInEmail = email; SignInError = None }, Cmd.none

let handleSignInSubmit (model: Model) : Model * Cmd<Msg> =
    let cmd =
        Cmd.OfAsync.perform
            (fun () -> Auth.signInWithMagicLink model.SignInEmail)
            ()
            SignInDone
    { model with IsAuthLoading = true }, cmd

let handleSignInDone (result: Result<unit, string>) (model: Model) : Model * Cmd<Msg> =
    match result with
    | Ok () ->
        { model with IsAuthLoading = false; SignInError = Some "Check your email for the magic link." }, Cmd.none
    | Error err ->
        { model with IsAuthLoading = false; SignInError = Some err }, Cmd.none

let handleSignOut (model: Model) : Model * Cmd<Msg> =
    let cmd = Cmd.OfAsync.attempt (fun () -> Auth.signOut ()) () (fun _ -> AuthReceived Auth.SignedOut)
    model, cmd
