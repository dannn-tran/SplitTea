# SplitTea — Terminology Spec

Defines the canonical product language for SplitTea.

This document is normative. Product copy and domain concepts should use these
terms consistently.

---

## Goals

- Use language that is clear to non-technical users
- Keep the domain model simple and flat
- Remove terms that mix domain identity with UI presentation
- Make each concept mean one thing

---

## Core Decision

SplitTea uses a single primary shared container concept:

```text
Space
```

A `Space` is the place where a set of people track shared expenses and
settlements.

Examples:
- roommates sharing household costs
- a trip with friends
- an event budget
- a shared lunch pool

---

## Canonical Terms

### 1. Space

The top-level shared container for all activity.

Definition:
- a named shared expense space
- has members
- has expenses
- has settlements
- has one append-only event history

Examples:
- `Roommates`
- `Bali Trip`
- `Wedding Budget`
- `Friday Lunch`

User-facing copy:
- `Create Space`
- `Join Space`
- `Leave Space`
- `Space Settings`

### 2. Member

A person participating in a space.

Definition:
- is relative to a specific space
- may or may not be linked to an authenticated user account
- can pay, owe, and receive money within a space

Relationship rules:
- a user can belong to many spaces or none
- a space can have many members
- a user has at most one membership in a given space

User-facing copy:
- `Members`
- `Add Member`

### 3. Expense

A recorded shared cost within a space.

Definition:
- always belongs to exactly one space
- has one payer
- has one split rule
- may optionally carry category/notes

User-facing copy:
- `Add Expense`
- `Edit Expense`
- `Delete Expense`

### 4. Settlement

A payment made to settle balances within a space.

Definition:
- always belongs to exactly one space
- moves money from one member to another

User-facing copy:
- `Record Settlement`

### 5. Invite

A way for a person to join a space.

Definition:
- grants access to a space
- links a user to an existing member or creates one during join flow

User-facing copy:
- `Invite`
- `Invite Link`
- `Join Space`

### 6. Activity

The chronological record of what happened in a space.

Definition:
- user-facing timeline/feed term
- backed by append-only domain events

User-facing copy:
- `Activity`
- `Recent Activity`

### 7. View

A UI presentation or filter over data in a space.

Definition:
- never a domain container
- never changes event ownership
- may filter by date, member, category, or status

Examples:
- `This Week`
- `This Month`
- `Trip Summary`
- `By Category`

User-facing copy:
- `View`
- `Filter`
- `This Week`
- `This Month`

---

## Domain Rules

### Flat Container Model

SplitTea has a flat top-level model:

```text
User
  belongs to many Spaces

Space
  has many Members
  has many Expenses
  has many Settlements
  has one Activity history
```

There is no nested `Space inside Group` model in the product vocabulary.

### Ownership Rules

- Every expense belongs to exactly one `Space`
- Every settlement belongs to exactly one `Space`
- Every member belongs to exactly one `Space`
- All actions occur within the currently opened `Space`

### Navigation Rule

The user enters a space first, then acts within it.

Implication:
- adding an expense does not ask the user to pick a context/container again
- recording a settlement does not ask the user to pick a context/container again

### View Rule

Date ranges, trip/month/week presets, and similar concerns are view-layer
concerns, not identity-layer concerns.

Implication:
- no `ContextTemplate`
- no `DateFrom` / `DateTo` as part of the core container identity

---

## Banned Terms

These terms should not be used in product vocabulary:

- `Context`
- `Group` as the primary user-facing container
- `Template` when referring to a domain container
- `Context Date Range`

---

## Naming Guidance

### User-Facing Language

Prefer:
- `Space`
- `Members`
- `Expenses`
- `Settlements`
- `Activity`
- `Invite`

Avoid:
- `Context`
- `Ledger`
- `Bucket`
- `Container`

---

## Example UX Copy

Good:
- `Create Space`
- `Join a Space`
- `Add Expense`
- `Record Settlement`
- `Members`
- `Activity`

Bad:
- `Create Context`
- `Add Expense to Context`
- `Choose Context`
- `Group Overview` when the UI otherwise says `Space`
