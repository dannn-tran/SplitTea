module Auth

open Fable.Core
open Fable.Core.JsInterop
open SupabaseClient

type AuthUser = {
    Id          : string
    Email       : string option
    AccessToken : string
}

type AuthEvent =
    | SignedIn  of AuthUser
    | SignedOut

let private mapUser (jsUser: obj) (session: obj) : AuthUser option =
    if isNull jsUser then None
    else
        let token =
            if isNull session || isNull session?access_token then ""
            else string session?access_token
        Some {
            Id          = jsUser?id
            Email       = if isNull jsUser?email then None else Some (string jsUser?email)
            AccessToken = token
        }

let signInWithMagicLink (email: string) : Async<Result<unit, string>> =
    async {
        if DevMode.isEnabled () then
            return Ok ()
        else
            let! result = supabase?auth?signInWithOtp({| email = email |}) |> Async.AwaitPromise
            if isNull result?error then
                return Ok ()
            else
                return Error (string result?error?message)
    }

let signOut () : Async<unit> =
    async {
        if not (DevMode.isEnabled ()) then
            let! _ = supabase?auth?signOut() |> Async.AwaitPromise
            ()
    }

let getUser () : Async<AuthUser option> =
    async {
        if DevMode.isEnabled () then
            return Some {
                Id          = string DevMode.fakeUserId
                Email       = Some DevMode.fakeUserEmail
                AccessToken = ""
            }
        else
            let! result = supabase?auth?getSession() |> Async.AwaitPromise
            let session = result?data?session
            let user    = if isNull session then null else session?user
            return mapUser user session
    }

// Returns an unsubscribe function.
let subscribe (callback: AuthEvent -> unit) : unit -> unit =
    if DevMode.isEnabled () then
        let user = {
            Id          = string DevMode.fakeUserId
            Email       = Some DevMode.fakeUserEmail
            AccessToken = ""
        }
        callback (SignedIn user)
        fun () -> ()
    else
        let sub =
            supabase?auth?onAuthStateChange(fun (event: string) (session: obj) ->
                if event = "SIGNED_IN" && not (isNull session) then
                    match mapUser (session?user) session with
                    | Some user -> callback (SignedIn user)
                    | None      -> callback SignedOut
                else
                    callback SignedOut
            )
        fun () -> sub?data?subscription?unsubscribe()
