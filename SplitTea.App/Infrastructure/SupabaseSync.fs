module SupabaseSync

open Fable.Core
open Fable.Core.JsInterop
open SupabaseClient

// Local camelCase storage format → Supabase snake_case columns.
// sequence is omitted — the DB trigger assigns it.
let private toSupabase (local: obj) : obj =
    createObj [
        "id"          ==> local?id
        "group_id"    ==> local?groupId
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
        "groupId"    ==> remote?group_id
        "sequence"   ==> remote?sequence
        "actorId"    ==> remote?actor_id
        "occurredAt" ==> remote?occurred_at
        "eventType"  ==> remote?event_type
        "payload"    ==> remote?payload
        "synced"     ==> true
    ]

let pushEvent (local: obj) : Async<Result<unit, string>> =
    async {
        let! result =
            supabase?from("events")?insert(toSupabase local)
            |> Async.AwaitPromise
        if isNull result?error then
            return Ok ()
        else
            return Error (string result?error?message)
    }

// Returns an unsubscribe function.
let subscribeGroup (groupId: string) (onEvent: obj -> unit) : unit -> unit =
    let filter  = sprintf "group_id=eq.%s" groupId
    let channel =
        supabase
            ?channel(sprintf "events:%s" groupId)
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
