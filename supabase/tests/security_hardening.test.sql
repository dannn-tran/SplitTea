-- pgTAP security hardening tests.
-- Run with: supabase test db

BEGIN;

SELECT plan(14);

-- ─── Fixtures ────────────────────────────────────────────────────────────────

-- Two synthetic user IDs (not real auth.users rows — pgTAP runs as postgres).
\set user_a_id  '''aaaaaaaa-0000-0000-0000-000000000001'''
\set user_b_id  '''bbbbbbbb-0000-0000-0000-000000000002'''
\set space_a_id '''aaaaaaaa-0000-0000-0001-000000000001'''
\set space_b_id '''bbbbbbbb-0000-0000-0001-000000000002'''
\set event_id_1 '''00000000-0000-0000-0000-000000000001'''
\set event_id_2 '''00000000-0000-0000-0000-000000000002'''
\set member_id  '''00000000-0000-0000-0000-000000000010'''

-- Seed space_access for user_a → space_a (as postgres / service_role, bypasses RLS)
INSERT INTO public.space_access (space_id, user_id)
    VALUES (:space_a_id::uuid, :user_a_id::uuid);

-- Seed one event for space_a (as postgres, bypasses RLS + trigger since auth.uid() is null)
INSERT INTO public.events (id, space_id, actor_id, occurred_at, event_type, payload)
    VALUES (
        :event_id_1::uuid,
        :space_a_id::uuid,
        :member_id::uuid,
        NOW(),
        'SpaceCreated',
        '{"name":"Test","currency":"GBP","createdBy":"00000000-0000-0000-0000-000000000010","categories":[]}'::jsonb
    );

-- ─── 1. events CHECK constraint: valid types pass ─────────────────────────────

SELECT lives_ok(
    $$ INSERT INTO public.events (id, space_id, actor_id, occurred_at, event_type, payload)
       VALUES (gen_random_uuid(), gen_random_uuid(), gen_random_uuid(), NOW(), 'SpaceRenamed',      '{}') $$,
    'SpaceRenamed passes CHECK constraint'
);
SELECT lives_ok(
    $$ INSERT INTO public.events (id, space_id, actor_id, occurred_at, event_type, payload)
       VALUES (gen_random_uuid(), gen_random_uuid(), gen_random_uuid(), NOW(), 'MemberRenamed',     '{}') $$,
    'MemberRenamed passes CHECK constraint'
);
SELECT lives_ok(
    $$ INSERT INTO public.events (id, space_id, actor_id, occurred_at, event_type, payload)
       VALUES (gen_random_uuid(), gen_random_uuid(), gen_random_uuid(), NOW(), 'CategoryAdded',     '{}') $$,
    'CategoryAdded passes CHECK constraint'
);
SELECT lives_ok(
    $$ INSERT INTO public.events (id, space_id, actor_id, occurred_at, event_type, payload)
       VALUES (gen_random_uuid(), gen_random_uuid(), gen_random_uuid(), NOW(), 'CategoryRenamed',   '{}') $$,
    'CategoryRenamed passes CHECK constraint'
);
SELECT lives_ok(
    $$ INSERT INTO public.events (id, space_id, actor_id, occurred_at, event_type, payload)
       VALUES (gen_random_uuid(), gen_random_uuid(), gen_random_uuid(), NOW(), 'CategoryArchived',  '{}') $$,
    'CategoryArchived passes CHECK constraint'
);

SELECT throws_ok(
    $$ INSERT INTO public.events (id, space_id, actor_id, occurred_at, event_type, payload)
       VALUES (gen_random_uuid(), gen_random_uuid(), gen_random_uuid(), NOW(), 'UnknownType', '{}') $$,
    '23514',
    NULL,
    'Unknown event_type rejected by CHECK constraint'
);

-- ─── 2. RLS: authenticated user cannot INSERT events directly ─────────────────

SELECT set_config('request.jwt.claims', format('{"sub":"%s"}', :user_a_id), true);
SET LOCAL ROLE authenticated;

SELECT throws_ok(
    format($$ INSERT INTO public.events (id, space_id, actor_id, occurred_at, event_type, payload)
              VALUES (gen_random_uuid(), %L::uuid, gen_random_uuid(), NOW(), 'ExpenseAdded', '{}') $$,
           :space_a_id),
    '42501',
    NULL,
    'Authenticated user cannot INSERT events directly (policy removed)'
);

-- ─── 3. RLS: user can SELECT only their own space events ──────────────────────

SELECT results_eq(
    format($$ SELECT COUNT(*)::int FROM public.events WHERE space_id = %L::uuid $$, :space_a_id),
    $$ VALUES (1) $$,
    'user_a sees their own space event'
);

SELECT results_eq(
    format($$ SELECT COUNT(*)::int FROM public.events WHERE space_id = %L::uuid $$, :space_b_id),
    $$ VALUES (0) $$,
    'user_a sees 0 events for a space they do not own'
);

-- ─── 4. space_access DELETE: user can delete their own row ───────────────────

SELECT lives_ok(
    format($$ DELETE FROM public.space_access WHERE space_id = %L::uuid AND user_id = %L::uuid $$,
           :space_a_id, :user_a_id),
    'user can delete their own space_access row'
);

-- Confirm deletion
SELECT results_eq(
    format($$ SELECT COUNT(*)::int FROM public.space_access WHERE space_id = %L::uuid $$, :space_a_id),
    $$ VALUES (0) $$,
    'space_access row is gone after delete'
);

-- ─── 5. space_access DELETE: user cannot delete another user's row ────────────

-- Re-seed space_access for user_b → space_b
RESET ROLE;
INSERT INTO public.space_access (space_id, user_id)
    VALUES (:space_b_id::uuid, :user_b_id::uuid);

-- Switch to user_a
SELECT set_config('request.jwt.claims', format('{"sub":"%s"}', :user_a_id), true);
SET LOCAL ROLE authenticated;

SELECT results_eq(
    format($$ SELECT COUNT(*)::int FROM public.space_access
              WHERE space_id = %L::uuid AND user_id = %L::uuid $$,
           :space_b_id, :user_b_id),
    $$ VALUES (0) $$,
    'user_a cannot see user_b space_access row (SELECT policy scoped to own rows)'
);

-- Attempting DELETE on a row user_a cannot see is a no-op (0 rows affected), not an error.
SELECT lives_ok(
    format($$ DELETE FROM public.space_access WHERE space_id = %L::uuid AND user_id = %L::uuid $$,
           :space_b_id, :user_b_id),
    'DELETE on another user row is a no-op (not an error)'
);

RESET ROLE;

SELECT * FROM finish();
ROLLBACK;
