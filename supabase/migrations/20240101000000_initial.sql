-- SplitTea: append-only event log schema

-- ─── Tables ──────────────────────────────────────────────────────────────────

-- Append-only event log. One row per domain event.
CREATE TABLE public.events (
    id           UUID        PRIMARY KEY,
    group_id     UUID        NOT NULL,
    sequence     BIGINT      NOT NULL DEFAULT 0,   -- assigned by trigger
    actor_id     UUID        NOT NULL,
    occurred_at  TIMESTAMPTZ NOT NULL,
    created_at   TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    event_type   TEXT        NOT NULL CHECK (event_type IN (
                     'GroupCreated', 'MemberAdded',
                     'ExpenseAdded', 'ExpenseCorrected', 'ExpenseDeleted',
                     'SettlementRecorded'
                 )),
    payload      JSONB       NOT NULL
);

-- Per-group monotonic sequence counter (written only by trigger).
CREATE TABLE public.group_sequences (
    group_id      UUID   PRIMARY KEY,
    next_sequence BIGINT NOT NULL DEFAULT 1
);

-- Auth users → groups access map (used for RLS).
-- A user is added here when they create a group, or when invited (future).
CREATE TABLE public.group_access (
    group_id  UUID        NOT NULL,
    user_id   UUID        NOT NULL REFERENCES auth.users (id) ON DELETE CASCADE,
    joined_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (group_id, user_id)
);

-- ─── Sequence trigger ─────────────────────────────────────────────────────────

-- Assigns a canonical, per-group monotonic sequence number to every inserted event.
-- SECURITY DEFINER so the trigger can write group_sequences even when RLS is on.
CREATE OR REPLACE FUNCTION public.assign_event_sequence()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
SET search_path = public
AS $$
BEGIN
    WITH updated AS (
        INSERT INTO public.group_sequences (group_id, next_sequence)
        VALUES (NEW.group_id, 2)
        ON CONFLICT (group_id) DO UPDATE
            SET next_sequence = group_sequences.next_sequence + 1
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
ALTER TABLE public.group_sequences ENABLE ROW LEVEL SECURITY;
ALTER TABLE public.group_access    ENABLE ROW LEVEL SECURITY;

-- events: members can read all events for their groups
CREATE POLICY "events_select" ON public.events
    FOR SELECT USING (
        EXISTS (
            SELECT 1 FROM public.group_access ga
            WHERE ga.group_id = events.group_id
              AND ga.user_id  = auth.uid()
        )
    );

-- events: members can append events to their groups
CREATE POLICY "events_insert" ON public.events
    FOR INSERT WITH CHECK (
        EXISTS (
            SELECT 1 FROM public.group_access ga
            WHERE ga.group_id = events.group_id
              AND ga.user_id  = auth.uid()
        )
    );

-- group_access: users see only their own rows
CREATE POLICY "group_access_select" ON public.group_access
    FOR SELECT USING (user_id = auth.uid());

-- group_access: a user may claim a group_id only if no events exist for it yet.
-- This lets the creator bootstrap access before inserting the GroupCreated event.
CREATE POLICY "group_access_insert_new_group" ON public.group_access
    FOR INSERT WITH CHECK (
        user_id = auth.uid()
        AND NOT EXISTS (
            SELECT 1 FROM public.events e WHERE e.group_id = group_access.group_id
        )
    );

-- group_sequences: read-only for members (written only by trigger)
CREATE POLICY "group_sequences_select" ON public.group_sequences
    FOR SELECT USING (
        EXISTS (
            SELECT 1 FROM public.group_access ga
            WHERE ga.group_id = group_sequences.group_id
              AND ga.user_id  = auth.uid()
        )
    );

-- ─── Realtime ─────────────────────────────────────────────────────────────────

-- Allow the events table to be streamed via Supabase Realtime.
ALTER PUBLICATION supabase_realtime ADD TABLE public.events;
