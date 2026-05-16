module Auth

open Fable.Core
open Fable.Core.JsInterop
open SupabaseClient

type AuthUser = {
    Id    : string
    Email : string option
}

type AuthEvent =
    | SignedIn  of AuthUser
    | SignedOut

let private mapUser (jsUser: obj) : AuthUser option =
    if isNull jsUser then None
    else
        Some {
            Id    = jsUser?id
            Email = if isNull jsUser?email then None else Some (string jsUser?email)
        }

let signInWithMagicLink (email: string) : Async<Result<unit, string>> =
    async {
        let! result = supabase?auth?signInWithOtp({| email = email |}) |> Async.AwaitPromise
        if isNull result?error then
            return Ok ()
        else
            return Error (string result?error?message)
    }

let signOut () : Async<unit> =
    async {
        let! _ = supabase?auth?signOut() |> Async.AwaitPromise
        ()
    }

let getUser () : Async<AuthUser option> =
    async {
        let! result = supabase?auth?getUser() |> Async.AwaitPromise
        return mapUser result?data?user
    }

// Returns an unsubscribe function.
let subscribe (callback: AuthEvent -> unit) : unit -> unit =
    let sub =
        supabase?auth?onAuthStateChange(fun (event: string) (session: obj) ->
            if event = "SIGNED_IN" && not (isNull session) then
                match mapUser (session?user) with
                | Some user -> callback (SignedIn user)
                | None      -> callback SignedOut
            else
                callback SignedOut
        )
    fun () -> sub?data?subscription?unsubscribe()
