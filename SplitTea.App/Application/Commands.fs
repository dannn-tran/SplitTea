module Commands

open SplitTea.Core

let private mkEnvelope (spaceId: SpaceId) (actorId: MemberId) (payload: 'P) : EventEnvelope<'P> =
    let ts = System.DateTimeOffset.UtcNow
    {
        Id         = EventId      (System.Guid.NewGuid())
        SpaceId    = spaceId
        Sequence   = 0L
        ActorId    = actorId
        OccurredAt = ts
        CreatedAt  = ts
        Payload    = payload
    }

let addExpense
    (spaceId:      SpaceId)
    (actorId:      MemberId)
    (description:  string)
    (paidAmount:   Amount)
    (paidCurrency: CurrencyCode)
    (exchangeRate: decimal option)
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
        ExchangeRate = exchangeRate
        PaidBy       = paidBy
        Split        = split
        Date         = date
        Category     = category
        Notes        = notes
    })
    |> Storage.saveEvent

let recordSettlement
    (spaceId:     SpaceId)
    (actorId:     MemberId)
    (from:        MemberId)
    (to':         MemberId)
    (amount:      Amount)
    (currency:    CurrencyCode)
    (exchangeRate: decimal option)
    (date:        System.DateOnly)
    (notes:       string option)
    : Async<unit> =
    SettlementRecorded (mkEnvelope spaceId actorId {
        SettlementId = SettlementId (System.Guid.NewGuid())
        From         = from
        To           = to'
        Amount       = amount
        Currency     = currency
        ExchangeRate = exchangeRate
        Date         = date
        Notes        = notes
    })
    |> Storage.saveEvent
