module Sync

open Fable.Core.JsInterop
open SplitTea.Core

// Push all locally-saved unsynced events to the Lambda write-proxy, marking each synced on success.
// Failures are silently skipped — the event stays pending and will retry next time.
let pushPending (authToken: string) : Async<unit> =
    async {
#if DEVMODE
        if DevMode.isEnabled () then
            ()
        else
#endif
            let! pending = IndexedDb.getPendingEvents ()
            for raw in pending do
                let! result = SupabaseSync.pushEvent raw authToken
                match result with
                | Ok ()    -> do! IndexedDb.markSynced (string raw?id)
                | Error _  -> ()
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
