-- SplitTea: append-only event log schema

-- ─── Tables ──────────────────────────────────────────────────────────────────

-- Append-only event log. One row per domain event.
CREATE TABLE public.events (
    id           UUID        PRIMARY KEY,
    space_id     UUID        NOT NULL,
    sequence     BIGINT      NOT NULL DEFAULT 0,   -- assigned by trigger
    actor_id     UUID        NOT NULL,
    occurred_at  TIMESTAMPTZ NOT NULL,
    created_at   TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    event_type   TEXT        NOT NULL CHECK (event_type IN (
                     'SpaceCreated', 'MemberAdded',
                     'ExpenseAdded', 'ExpenseCorrected', 'ExpenseDeleted',
                     'SettlementRecorded'
                 )),
    payload      JSONB       NOT NULL
);

-- Per-space monotonic sequence counter (written only by trigger).
CREATE TABLE public.space_sequences (
    space_id      UUID   PRIMARY KEY,
    next_sequence BIGINT NOT NULL DEFAULT 1
);

-- Auth users → spaces access map (used for RLS).
-- A user is added here when they create a space, or when invited (future).
CREATE TABLE public.space_access (
    space_id  UUID        NOT NULL,
    user_id   UUID        NOT NULL REFERENCES auth.users (id) ON DELETE CASCADE,
    joined_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (space_id, user_id)
);

-- ─── Sequence trigger ─────────────────────────────────────────────────────────

-- Assigns a canonical, per-space monotonic sequence number to every inserted event.
-- SECURITY DEFINER so the trigger can write space_sequences even when RLS is on.
CREATE OR REPLACE FUNCTION public.assign_event_sequence()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
SET search_path = public
AS $$
BEGIN
    WITH updated AS (
        INSERT INTO public.space_sequences (space_id, next_sequence)
        VALUES (NEW.space_id, 2)
        ON CONFLICT (space_id) DO UPDATE
            SET next_sequence = space_sequences.next_sequence + 1
        RETURNING next_sequence - 1 AS seq
    )
    SELECT seq INTO NEW.sequence FROM updated;
    RETURN NEW;
END;
$$;

CREATE TRIGGER events_assign_sequence
BEFORE INSERT ON public.events
FOR EACH ROW EXECUTE FUNCTION public.assign_event_sequence();

-- ─── Row-Level Security ───────────────────────────────────────────────────────

ALTER TABLE public.events          ENABLE ROW LEVEL SECURITY;
ALTER TABLE public.space_sequences ENABLE ROW LEVEL SECURITY;
ALTER TABLE public.space_access    ENABLE ROW LEVEL SECURITY;

-- events: members can read all events for their spaces
CREATE POLICY "events_select" ON public.events
    FOR SELECT USING (
        EXISTS (
            SELECT 1 FROM public.space_access sa
            WHERE sa.space_id = events.space_id
              AND sa.user_id  = auth.uid()
        )
    );

-- events: members can append events to their spaces
CREATE POLICY "events_insert" ON public.events
    FOR INSERT WITH CHECK (
        EXISTS (
            SELECT 1 FROM public.space_access sa
            WHERE sa.space_id = events.space_id
              AND sa.user_id  = auth.uid()
        )
    );

-- space_access: users see only their own rows
CREATE POLICY "space_access_select" ON public.space_access
    FOR SELECT USING (user_id = auth.uid());

-- space_access: a user may claim a space_id only if no events exist for it yet.
-- This lets the creator bootstrap access before inserting the SpaceCreated event.
CREATE POLICY "space_access_insert_new_space" ON public.space_access
    FOR INSERT WITH CHECK (
        user_id = auth.uid()
        AND NOT EXISTS (
            SELECT 1 FROM public.events e WHERE e.space_id = space_access.space_id
        )
    );

-- space_sequences: read-only for members (written only by trigger)
CREATE POLICY "space_sequences_select" ON public.space_sequences
    FOR SELECT USING (
        EXISTS (
            SELECT 1 FROM public.space_access sa
            WHERE sa.space_id = space_sequences.space_id
              AND sa.user_id  = auth.uid()
        )
    );

-- ─── Realtime ─────────────────────────────────────────────────────────────────

-- Allow the events table to be streamed via Supabase Realtime.
ALTER PUBLICATION supabase_realtime ADD TABLE public.events;
