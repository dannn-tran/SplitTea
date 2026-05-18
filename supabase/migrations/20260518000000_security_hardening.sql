-- Security hardening: actor stamping, lock down direct event writes,
-- fix event_type constraint, space_access delete policy.

-- 1. Stamp actor_id with auth.uid() on every authenticated insert.
--    Skips stamping when auth.uid() is null (service_role / Lambda writes).
CREATE OR REPLACE FUNCTION public.stamp_actor_id()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
SET search_path = public
AS $$
BEGIN
    IF auth.uid() IS NOT NULL THEN
        NEW.actor_id := auth.uid();
    END IF;
    RETURN NEW;
END;
$$;

CREATE TRIGGER events_stamp_actor
BEFORE INSERT ON public.events
FOR EACH ROW EXECUTE FUNCTION public.stamp_actor_id();

-- 2. Remove the authenticated-user INSERT policy on events.
--    Lambda writes via service_role (bypasses RLS), so those still work.
--    Direct client inserts are now denied.
DROP POLICY IF EXISTS "events_insert" ON public.events;

-- 3. Fix the event_type CHECK constraint — original was missing 5 types,
--    causing SpaceRenamed, MemberRenamed, and all Category events to fail sync.
ALTER TABLE public.events DROP CONSTRAINT IF EXISTS events_event_type_check;

ALTER TABLE public.events ADD CONSTRAINT events_event_type_check
    CHECK (event_type IN (
        'SpaceCreated',     'SpaceRenamed',
        'MemberAdded',      'MemberRenamed',
        'CategoryAdded',    'CategoryRenamed',  'CategoryArchived',
        'ExpenseAdded',     'ExpenseCorrected', 'ExpenseDeleted',
        'SettlementRecorded'
    ));

-- 4. Allow users to delete their own space_access row (needed by deleteSpace).
CREATE POLICY "space_access_delete_own" ON public.space_access
    FOR DELETE USING (user_id = auth.uid());
