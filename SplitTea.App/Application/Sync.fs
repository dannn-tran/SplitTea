module Sync

open Fable.Core.JsInterop
open SplitTea.Core

type PushOutcome =
    | Pushed
    | TokenExpired       // 401 — caller should refresh and retry
    | PermanentRejection // 400/403/422 — event is invalid; rebase needed
    | TransientFailure   // 5xx / network — retry next cycle

// Push all locally-saved unsynced events to the Lambda write-proxy.
// Returns per-event outcomes so the caller can trigger rebase on permanent rejections.
let pushPending (authToken: string) : Async<PushOutcome list> =
    async {
#if DEVMODE
        if DevMode.isEnabled () then
            return []
        else
#endif
        let! pending = IndexedDb.getPendingEvents ()
        let outcomes = ResizeArray()
        for raw in pending do
            let! result = SupabaseSync.pushEvent raw authToken
            match result with
            | Ok () ->
                do! IndexedDb.markSynced (string raw?id)
                outcomes.Add Pushed
            | Error (401, _) ->
                outcomes.Add TokenExpired
            | Error (status, _) when status >= 400 && status < 500 ->
                do! IndexedDb.markConflicted (string raw?id)
                outcomes.Add PermanentRejection
            | Error _ ->
                outcomes.Add TransientFailure
        return outcomes |> Seq.toList
    }

// Subscribe to Supabase Realtime for a space.
// Each inbound INSERT is saved locally (marked synced=true) then onNewEvent () is called.
// Returns an unsubscribe function.
let subscribeSpace (spaceId: SpaceId) (onNewEvent: unit -> unit) : unit -> unit =
#if DEVMODE
    if DevMode.isEnabled () then
        fun () -> ()
    else
#endif
        let (SpaceId g) = spaceId
        SupabaseSync.subscribeSpace (string g) (fun normalized ->
            async {
                do! IndexedDb.saveEvent normalized
                onNewEvent ()
            } |> Async.StartImmediate
        )
