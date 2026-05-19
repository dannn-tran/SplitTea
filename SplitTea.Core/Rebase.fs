namespace SplitTea.Core

type RebaseConflict = {
    Event  : SpaceEvent
    Errors : ValidationError list
}

module Rebase =
    // Given the authoritative synced event log and a list of pending local events (in
    // occurredAt order), compute the display state and any conflicts.
    //
    // The rebase loop accumulates state incrementally: if a pending event is invalid,
    // the accumulated state is NOT updated, so subsequent events that depended on it
    // will also fail validation — cascading conflicts surface naturally without explicit
    // tracking.
    let apply (syncedEvents: SpaceEvent list) (pendingEvents: SpaceEvent list) : SpaceState * RebaseConflict list =
        let serverState = Reducer.replayEvents syncedEvents
        let finalState, reversedConflicts =
            ((serverState, []), pendingEvents)
            ||> List.fold (fun (accumulated, conflicts) event ->
                match Validation.validateEvent accumulated event with
                | Ok _       -> Reducer.applyEvent accumulated event, conflicts
                | Error errs -> accumulated, { Event = event; Errors = errs } :: conflicts
            )
        finalState, List.rev reversedConflicts
