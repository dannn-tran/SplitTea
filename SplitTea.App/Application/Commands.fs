module Commands

open SplitTea.Core

let private mkEnvelope (groupId: GroupId) (actorId: MemberId) (payload: 'P) : EventEnvelope<'P> =
    let ts = System.DateTimeOffset.UtcNow
    {
        Id         = EventId      (System.Guid.NewGuid())
        GroupId    = groupId
        Sequence   = 0L
        ActorId    = actorId
        OccurredAt = ts
        CreatedAt  = ts
        Payload    = payload
    }

let createContext
    (groupId:  GroupId)
    (actorId:  MemberId)
    (name:     string)
    (template: ContextTemplate)
    (members:  MemberId list option)
    (dateFrom: System.DateOnly option)
    (dateTo:   System.DateOnly option)
    : Async<unit> =
    ContextCreated (mkEnvelope groupId actorId {
        ContextId = ContextId (System.Guid.NewGuid())
        Name      = name
        Template  = template
        Members   = members
        DateFrom  = dateFrom
        DateTo    = dateTo
    })
    |> Storage.saveEvent

let addExpense
    (groupId:      GroupId)
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
    (contextId:    ContextId option)
    : Async<unit> =
    ExpenseAdded (mkEnvelope groupId actorId {
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
        ContextId    = contextId
    })
    |> Storage.saveEvent

let recordSettlement
    (groupId:     GroupId)
    (actorId:     MemberId)
    (from:        MemberId)
    (to':         MemberId)
    (amount:      Amount)
    (currency:    CurrencyCode)
    (exchangeRate: decimal option)
    (date:        System.DateOnly)
    (notes:       string option)
    : Async<unit> =
    SettlementRecorded (mkEnvelope groupId actorId {
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
