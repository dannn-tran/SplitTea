module SupabaseSync

open Fable.Core
open Fable.Core.JsInterop
open SupabaseClient

// Local camelCase storage format → Supabase snake_case columns.
// sequence is omitted — the DB trigger assigns it.
let private toSupabase (local: obj) : obj =
    createObj [
        "id"          ==> local?id
        "space_id"    ==> local?spaceId
        "actor_id"    ==> local?actorId
        "occurred_at" ==> local?occurredAt
        "event_type"  ==> local?eventType
        "payload"     ==> local?payload
    ]

// Supabase snake_case row → local camelCase storage format.
// Marks synced=true so the event is not re-pushed.
let private fromSupabase (remote: obj) : obj =
    createObj [
        "id"         ==> remote?id
        "spaceId"    ==> remote?space_id
        "sequence"   ==> remote?sequence
        "actorId"    ==> remote?actor_id
        "occurredAt" ==> remote?occurred_at
        "eventType"  ==> remote?event_type
        "payload"    ==> remote?payload
        "synced"     ==> true
    ]

let private lambdaUrl : string = emitJsExpr () "import.meta.env.VITE_LAMBDA_URL"
let private jsonStringify (o: obj) : string = emitJsExpr o "JSON.stringify($0)"

// Posts the raw local event JSON to the Lambda write-proxy for validation + storage.
// Error carries (httpStatus, body) so callers can distinguish 4xx (permanent) from 5xx (transient).
// Network errors use status 0.
let pushEvent (local: obj) (authToken: string) : Async<Result<unit, int * string>> =
    async {
        try
            let init =
                createObj [
                    "method"  ==> "POST"
                    "headers" ==> createObj [
                        "Content-Type"  ==> "application/json"
                        "Authorization" ==> ("Bearer " + authToken)
                    ]
                    "body"    ==> jsonStringify local
                ]
            let url = lambdaUrl + "/events"
            let! response = emitJsExpr (url, init) "fetch($0, $1)" |> Async.AwaitPromise
            let ok : bool = response?ok
            if ok then
                return Ok ()
            else
                let status : int = response?status
                let! text = (response?text() : JS.Promise<string>) |> Async.AwaitPromise
                return Error (status, text)
        with ex ->
            return Error (0, ex.Message)
    }

// Removes the current user's space_access row, revoking their server-side access.
// Uses the authenticated Supabase client so RLS (space_access_delete_own policy) applies.
let removeSpaceAccess (spaceId: string) : Async<unit> =
    async {
#if DEVMODE
        if DevMode.isEnabled () then ()
        else
#endif
        if not (isNull supabase) then
            let! _ =
                supabase?from("space_access")?delete()?eq("space_id", spaceId)
                |> Async.AwaitPromise
            ()
    }

// Returns an unsubscribe function.
let subscribeSpace (spaceId: string) (onEvent: obj -> unit) : unit -> unit =
    let filter  = sprintf "space_id=eq.%s" spaceId
    let channel =
        supabase
            ?channel(sprintf "events:%s" spaceId)
            ?on(
                "postgres_changes",
                createObj [
                    "event"  ==> "INSERT"
                    "schema" ==> "public"
                    "table"  ==> "events"
                    "filter" ==> filter
                ],
                fun (payload: obj) ->
                    // payload.new is a reserved-word property name
                    let row : obj = emitJsExpr payload "$0['new']"
                    if not (isNull row) then
                        onEvent (fromSupabase row)
            )
            ?subscribe()
    fun () -> supabase?removeChannel(channel) |> ignore
