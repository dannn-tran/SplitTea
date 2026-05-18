# Offline-First Sync and Conflict Resolution Design

## Guiding Principles

**The device is sovereign.** Every user action must succeed immediately, without network. The network is a delivery mechanism, not a gatekeeper. An app that blocks on connectivity feels broken — not slow, broken.

**Events are primary, state is derived.** Rather than synchronising mutable state (hard: requires CRDTs or OT), we synchronise an immutable, append-only event log (tractable: replaying the same events always produces the same state). The event log is the source of truth; the rendered state is a projection of it.

**The server is the arbiter of ordering and validity, not a prerequisite for action.** Clients write events locally at any time. The server validates them, assigns authoritative sequence numbers, and broadcasts them to other clients. When the server disagrees with the client, the server wins — but the user is always told.

**Conflicts surface explicitly, never silently.** A conflicted event is never silently discarded or silently kept. The user is always informed when one of their actions could not be applied.

---

## Event Lifecycle

Every event passes through three states:

```
[pending]  → written locally, not yet confirmed by server
[synced]   → accepted and sequenced by server
[conflicted] → permanently invalid against server state; excluded from display
```

### Write path

1. User takes an action.
2. Event is written to local IndexedDB as `synced = false` (pending).
3. UI renders immediately from local state including the pending event.
4. Async: event is pushed to the Lambda write-proxy.
   - **200**: mark `synced = true`. No further action.
   - **401**: token expired. Refresh token and retry.
   - **400 / 403 / 422**: permanent rejection. Trigger rebase (see below).
   - **5xx / network failure**: transient. Retry on next sync cycle.

### Pull path

1. Server event arrives via Supabase Realtime (INSERT on `events` table).
2. Saved to IndexedDB as `synced = true`.
3. Rebase is triggered (see below).

### Sync trigger points

Pending events are pushed to the server:
- On app load (after auth).
- When a new space is loaded.
- When the browser fires the `online` event (reconnect after offline).

Rebase runs:
- After every successful pull (Realtime event received).
- After any push that results in a permanent rejection.

---

## The Rebase Model

When server state changes, pending local events must be re-evaluated against the new base. This is the same problem `git rebase` solves: given a new base commit, re-apply your local commits on top of it, and surface any that no longer apply cleanly.

### Algorithm

```
serverState  = replay(all events where synced = true)
accumulated  = serverState
conflicts    = []

for each pending event, ordered by occurredAt:
    errors = Validation.validateEvent accumulated event
    if errors is empty:
        accumulated = Reducer.applyEvent accumulated event
    else:
        mark event as conflicted in IndexedDB
        conflicts.append(event, errors)
        // do NOT update accumulated — subsequent events see state without this one

displayState = accumulated
```

### Why this handles cascading naturally

The accumulated state is only updated when an event passes validation. If event A is conflicted, accumulated does not reflect A's effects when B is evaluated. If B depended on A — for example, B corrects an expense that A created — B will fail `checkExpense`, and B becomes conflicted too. No explicit cascade tracking is needed.

### Invariant

> `displayState` is always equal to what the server would compute if it accepted all currently-pending events on top of current server state.

If a pending event would be rejected by the server, it is excluded from `displayState` before it even reaches the server.

---

## Conflict Resolution Policy

**Server wins.** When a pending local event is invalid against server state, the server's version of reality is preserved and the local event is discarded.

This is the appropriate default for a shared financial ledger:

- Deletion is a strong, intentional signal. Automatically "un-deleting" to preserve a concurrent edit would violate the intent of whoever deleted.
- Merging two concurrent corrections to the same expense is semantically ambiguous — which amount is correct?
- Users of a shared expense tracker expect that what they see matches what others see. Diverging quietly is worse than a visible conflict notification.

### User notification

When a conflict occurs, the user is shown:
- Which event conflicted (e.g., "Edit to 'Dinner' could not be applied").
- Why (e.g., "This expense was deleted by another user").
- A dismiss action that removes the conflicted event from IndexedDB permanently.

Conflicted events are never retried. They are kept in IndexedDB (as `synced = "conflicted"`) until dismissed, so the notification can be reconstructed after a page reload.

---

## What the System Guarantees

| Property | Guarantee |
|---|---|
| Offline writes | Always succeed locally |
| Immediate feedback | User sees their action reflected in UI before any network round-trip |
| No silent data loss | Every conflict is surfaced to the user |
| No invalid state | Display state is always internally consistent (valid pending rebased on server state) |
| No divergence | Conflicted events never reach the server |
| Idempotent push | Retrying a pending event that was already accepted is safe (server deduplicates by EventId) |

---

## What the System Does Not Guarantee

- **Automatic conflict resolution.** No field-level merging, no CRDTs. Conflicts require human resolution (dismiss the conflicted event).
- **Causal ordering across devices.** Two clients writing simultaneously may have their events sequenced in any order by the server. The resulting state is deterministic but may not match either client's local order.
- **Offline reads of remote data.** If a client has never loaded a space while online, it cannot see events from other clients while offline.

---

## Architecture Boundary Responsibilities

| Layer | Responsibility |
|---|---|
| `SplitTea.Core` (F#, shared) | Validation, state reduction. Runs identically on Lambda and client (via Fable). |
| `SplitTea.Lambda` (.NET) | Authoritative validation, sequence assignment, `space_access` management. |
| `IndexedDb` (browser) | Durable local event store. Single source of display state. |
| `SupabaseSync` (client) | Push events to Lambda; subscribe to Realtime. |
| `Storage` (client) | Orchestrates rebase. Owns the read/write contract over IndexedDb. |
| `Supabase` (DB + Realtime) | Persistent event log, sequence via DB trigger, realtime fan-out. |

The reuse of `SplitTea.Core` on both sides is the critical property: validation runs server-side (authoritative) and client-side (proactive conflict detection), from the same F# code. Adding a validation rule automatically enforces it in both places.
