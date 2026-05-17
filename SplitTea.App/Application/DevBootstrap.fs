module DevBootstrap

open SplitTea.Core

#if DEVMODE
let private mkEnvelope (spaceId: SpaceId) (actorId: MemberId) (payload: 'P) : EventEnvelope<'P> =
    let ts = System.DateTimeOffset.UtcNow
    {
        Id         = EventId (System.Guid.NewGuid())
        SpaceId    = spaceId
        Sequence   = 0L
        ActorId    = actorId
        OccurredAt = ts
        CreatedAt  = ts
        Payload    = payload
    }

let createLocalSpace () : Async<SpaceId> =
    async {
        let spaceId = SpaceId (System.Guid.NewGuid())
        let aliceId = MemberId (System.Guid.NewGuid())
        let bobId   = MemberId (System.Guid.NewGuid())
        let carolId = MemberId (System.Guid.NewGuid())

        do!
            SpaceCreated (mkEnvelope spaceId aliceId {
                Name       = DevMode.fakeSpaceName
                Currency   = "SGD"
                CreatedBy  = aliceId
                Categories = Commands.defaultCategories
            })
            |> Storage.saveEvent

        for (mid, name) in [ aliceId, "Alice"; bobId, "Bob"; carolId, "Carol" ] do
            do!
                MemberAdded (mkEnvelope spaceId aliceId {
                    Member = { Id = mid; DisplayName = name; UserId = None }
                })
                |> Storage.saveEvent

        return spaceId
    }
#endif
