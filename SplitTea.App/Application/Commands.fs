module Commands

open SplitTea.Core

let defaultCategories = [
    "Food & Drink"
    "Transport"
    "Accommodation"
    "Entertainment"
    "Shopping"
    "Utilities"
    "Health"
    "Other"
]

let private mkEnvelope (spaceId: SpaceId) (actorId: MemberId) (payload: 'P) : EventEnvelope<'P> =
    let ts = System.DateTimeOffset.UtcNow
    {
        Id         = EventId      (System.Guid.NewGuid())
        SpaceId    = spaceId
        Sequence   = 0L
        ActorId    = actorId
        OccurredAt = ts
        Payload    = payload
    }

let createSpace (name: string) (currency: string) (memberName: string) (userId: UserId option) : Async<SpaceId> =
    async {
        let spaceId  = SpaceId  (System.Guid.NewGuid())
        let memberId = MemberId (System.Guid.NewGuid())
        do!
            SpaceCreated (mkEnvelope spaceId memberId {
                Name       = name.Trim()
                Currency   = currency.Trim().ToUpper()
                CreatedBy  = memberId
                Categories = defaultCategories
            })
            |> Storage.saveEvent
        do!
            MemberAdded (mkEnvelope spaceId memberId {
                Member = { Id = memberId; DisplayName = memberName.Trim(); UserId = userId }
            })
            |> Storage.saveEvent
        return spaceId
    }

let addExpense
    (spaceId:      SpaceId)
    (actorId:      MemberId)
    (description:  string)
    (paidAmount:   Amount)
    (paidCurrency: CurrencyCode)
    (paidBy:       MemberId)
    (split:        Split)
    (date:         System.DateOnly)
    (category:     string option)
    (notes:        string option)
    : Async<unit> =
    ExpenseAdded (mkEnvelope spaceId actorId {
        ExpenseId    = ExpenseId (System.Guid.NewGuid())
        Description  = description
        PaidAmount   = paidAmount
        PaidCurrency = paidCurrency
        PaidBy       = paidBy
        Split        = split
        Date         = date
        Category     = category
        Notes        = notes
    })
    |> Storage.saveEvent

let renameSpace (spaceId: SpaceId) (actorId: MemberId) (newName: string) : Async<unit> =
    SpaceRenamed (mkEnvelope spaceId actorId { NewName = newName.Trim() })
    |> Storage.saveEvent

let renameMember (spaceId: SpaceId) (actorId: MemberId) (memberId: MemberId) (newName: string) : Async<unit> =
    MemberRenamed (mkEnvelope spaceId actorId { MemberId = memberId; NewName = newName.Trim() })
    |> Storage.saveEvent

let addCategory
    (spaceId: SpaceId)
    (actorId: MemberId)
    (name: string)
    : Async<unit> =
    CategoryAdded (mkEnvelope spaceId actorId { Name = name.Trim() })
    |> Storage.saveEvent

let renameCategory
    (spaceId: SpaceId)
    (actorId: MemberId)
    (oldName: string)
    (newName: string)
    : Async<unit> =
    CategoryRenamed (mkEnvelope spaceId actorId {
        OldName = oldName.Trim()
        NewName = newName.Trim()
    })
    |> Storage.saveEvent

let archiveCategory
    (spaceId: SpaceId)
    (actorId: MemberId)
    (name: string)
    : Async<unit> =
    CategoryArchived (mkEnvelope spaceId actorId { Name = name.Trim() })
    |> Storage.saveEvent

let correctExpense
    (spaceId:      SpaceId)
    (actorId:      MemberId)
    (original:     ExpenseState)
    (description:  string)
    (paidAmount:   Amount)
    (paidCurrency: CurrencyCode)
    (paidBy:       MemberId)
    (split:        Split)
    (date:         System.DateOnly)
    (category:     string option)
    (notes:        string option)
    : Async<unit> =
    let diffOpt eq newVal oldVal = if eq newVal oldVal then None else Some newVal
    let diffPatch newOpt oldOpt =
        match newOpt, oldOpt with
        | n, o when n = o -> Unchanged
        | Some v, _       -> SetTo v
        | None, _         -> Clear
    ExpenseCorrected (mkEnvelope spaceId actorId {
        OriginalExpenseId = original.ExpenseId
        Description       = diffOpt (=) description original.Description
        PaidAmount        = diffOpt (=) paidAmount   original.PaidAmount
        PaidCurrency      = diffOpt (=) paidCurrency original.PaidCurrency
        PaidBy            = diffOpt (=) paidBy        original.PaidBy
        Split             = diffOpt (=) split          original.Split
        Date              = diffOpt (=) date           original.Date
        Category          = diffPatch category          original.Category
        Notes             = diffPatch notes             original.Notes
        Reason            = None
    })
    |> Storage.saveEvent

let deleteExpense
    (spaceId:   SpaceId)
    (actorId:   MemberId)
    (expenseId: ExpenseId)
    : Async<unit> =
    ExpenseDeleted (mkEnvelope spaceId actorId {
        ExpenseId = expenseId
        Reason    = None
    })
    |> Storage.saveEvent

let recordSettlement
    (spaceId:  SpaceId)
    (actorId:  MemberId)
    (from:     MemberId)
    (to':      MemberId)
    (payments: SettlementLeg list)
    (date:     System.DateOnly)
    (notes:    string option)
    : Async<unit> =
    SettlementRecorded (mkEnvelope spaceId actorId {
        SettlementId = SettlementId (System.Guid.NewGuid())
        From         = from
        To           = to'
        Payments     = payments
        Date         = date
        Notes        = notes
    })
    |> Storage.saveEvent
