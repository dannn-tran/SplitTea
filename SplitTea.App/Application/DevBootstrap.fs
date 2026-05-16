module DevBootstrap

open SplitTea.Core

#if DEVMODE
let private mkEnvelope (groupId: GroupId) (actorId: MemberId) (payload: 'P) : EventEnvelope<'P> =
    let ts = System.DateTimeOffset.UtcNow
    {
        Id         = EventId (System.Guid.NewGuid())
        GroupId    = groupId
        Sequence   = 0L
        ActorId    = actorId
        OccurredAt = ts
        CreatedAt  = ts
        Payload    = payload
    }

let createLocalGroup () : Async<GroupId> =
    async {
        let groupId = GroupId (System.Guid.NewGuid())
        let memberId = MemberId (System.Guid.NewGuid())
        let userId = UserId DevMode.fakeUserId

        do!
            GroupCreated (mkEnvelope groupId memberId {
                Name = "Local Test Group"
                Currency = "SGD"
                CreatedBy = memberId
            })
            |> Storage.saveEvent

        do!
            MemberAdded (mkEnvelope groupId memberId {
                Member = {
                    Id = memberId
                    DisplayName = DevMode.fakeMemberName
                    UserId = Some userId
                }
            })
            |> Storage.saveEvent

        return groupId
    }
#endif
