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
        let memberId = MemberId (System.Guid.NewGuid())
        let userId = UserId DevMode.fakeUserId

        do!
            SpaceCreated (mkEnvelope spaceId memberId {
                Name       = DevMode.fakeSpaceName
                Currency   = "SGD"
                CreatedBy  = memberId
                Categories = Commands.defaultCategories
            })
            |> Storage.saveEvent

        do!
            MemberAdded (mkEnvelope spaceId memberId {
                Member = {
                    Id = memberId
                    DisplayName = DevMode.fakeMemberName
                    UserId = Some userId
                }
            })
            |> Storage.saveEvent

        return spaceId
    }
#endif
