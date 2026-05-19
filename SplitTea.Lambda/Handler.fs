module SplitTea.Lambda.Handler

open System
open System.Text.Json
open Amazon.Lambda.Core
open Amazon.Lambda.APIGatewayEvents
open SplitTea.Core

[<assembly: LambdaSerializer(typeof<Amazon.Lambda.Serialization.SystemTextJson.DefaultLambdaJsonSerializer>)>]
do ()

// Env vars expected at runtime.
let private supabaseUrl    = Environment.GetEnvironmentVariable "SUPABASE_URL"
let private jwksUrl        = Environment.GetEnvironmentVariable "SUPABASE_JWKS_URL"
let private connString     = Environment.GetEnvironmentVariable "SUPABASE_DB_CONNECTION_STRING"

let private jsonOpts =
    let o = JsonSerializerOptions()
    o.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
    o

let private respond (status: int) (body: string) : APIGatewayHttpApiV2ProxyResponse =
    APIGatewayHttpApiV2ProxyResponse(
        StatusCode = status,
        Body       = body,
        Headers    = dict [ "Content-Type", "application/json" ]
    )

let private ok ()       = respond 200 """{"ok":true}"""
let private badRequest  = respond 400
let private unauthorized = respond 401 """{"error":"Unauthorized"}"""
let private forbidden    = respond 403 """{"error":"Forbidden"}"""
let private unprocessable (errors: string) = respond 422 errors
let private serverError (msg: string) = respond 500 (JsonSerializer.Serialize({| error = msg |}, jsonOpts))

// Extract Bearer token from Authorization header.
let private extractToken (req: APIGatewayHttpApiV2ProxyRequest) : string option =
    let mutable v = ""
    if req.Headers <> null && req.Headers.TryGetValue("authorization", &v) then
        let parts = v.Split(' ')
        if parts.Length = 2 && parts.[0].ToLowerInvariant() = "bearer" then Some parts.[1]
        else None
    else None

// Find the MemberId for a given UserId within a SpaceState.
let private findMemberId (state: SpaceState) (userId: UserId) : MemberId option =
    state.Members
    |> Map.tryFindKey (fun _ m -> m.UserId = Some userId)

let private stampActor = SpaceEvent.withActorId

// Extracts spaceId from paths like /spaces/{guid}/balances.
let private tryParseBalancesPath (path: string) : SpaceId option =
    if path = null then None
    else
        let parts = path.TrimStart('/').Split('/')
        if parts.Length = 3 && parts.[0] = "spaces" && parts.[2] = "balances" then
            match Guid.TryParse parts.[1] with
            | true, g -> Some (SpaceId g)
            | _       -> None
        else None

let private handleGetBalances (req: APIGatewayHttpApiV2ProxyRequest) (userId: UserId) (spaceId: SpaceId) =
    async {
        let! hasAccess = EventRepository.userHasAccess connString spaceId userId
        if not hasAccess then
            return forbidden
        else
            let! state = EventRepository.loadSpaceState connString spaceId
            let positions = Projections.computeNetPositions state
            let settlements = Projections.computeMinimumSettlements positions
            let memberName (mid: MemberId) =
                let (MemberId g) = mid
                state.Members
                |> Map.tryFind mid
                |> Option.map (fun m -> m.DisplayName)
                |> Option.defaultValue (string g)
            let result = {|
                currency    = state.Currency
                positions   = positions |> List.map (fun p ->
                    let (MemberId g) = p.MemberId
                    {| memberId = string g; memberName = memberName p.MemberId; amount = p.Amount |})
                settlements = settlements |> List.map (fun s ->
                    let (MemberId fg) = s.From
                    let (MemberId tg) = s.To
                    {| from = string fg; fromName = memberName s.From
                       ``to`` = string tg; toName = memberName s.To
                       amount = s.Amount |})
            |}
            return respond 200 (JsonSerializer.Serialize(result, jsonOpts))
    }

let handler (req: APIGatewayHttpApiV2ProxyRequest) (_ctx: ILambdaContext) : Async<APIGatewayHttpApiV2ProxyResponse> =
    async {
        // 1. Verify JWT.
        match extractToken req with
        | None -> return unauthorized
        | Some token ->
            let! authResult = JwtVerifier.verify jwksUrl supabaseUrl token
            match authResult with
            | Error _ -> return unauthorized
            | Ok userIdStr ->
                let userId = UserId (Guid.Parse userIdStr)

                // 1a. Route GET /spaces/:id/balances before reading the body.
                let httpCtx = if req.RequestContext <> null then req.RequestContext.Http else null
                let method  = if httpCtx <> null then httpCtx.Method else null
                let path    = if httpCtx <> null then httpCtx.Path   else null
                match method, tryParseBalancesPath path with
                | "GET", Some spaceId ->
                    return! handleGetBalances req userId spaceId
                | _ ->

                // 2. Parse event from request body.
                if String.IsNullOrWhiteSpace req.Body then
                    return badRequest """{"error":"Empty body"}"""
                else
                    match Serde.decodeEventJson req.Body with
                    | Error err ->
                        return badRequest (JsonSerializer.Serialize({| error = "Invalid event JSON"; detail = err |}, jsonOpts))
                    | Ok event ->

                        // 3. Determine spaceId and load current state.
                        let spaceId = SpaceEvent.getSpaceId event
                        let isSpaceCreated = match event with SpaceCreated _ -> true | _ -> false

                        // 3a. Access gate: non-SpaceCreated events require an existing space_access row.
                        let! hasAccess =
                            if isSpaceCreated then async { return true }
                            else EventRepository.userHasAccess connString spaceId userId

                        if not hasAccess then
                            return forbidden
                        else

                        let! state = EventRepository.loadSpaceState connString spaceId

                        // 4. Resolve actorId from state.
                        //    Fall back to clientActorId only during bootstrap (space has no members yet)
                        //    so the creator can add the first MemberAdded event.
                        //    For all other cases, fail-closed — the user must be a recognized member.
                        let clientActorId = SpaceEvent.getActorId event

                        let actorIdOpt =
                            match findMemberId state userId with
                            | Some id -> Some id
                            | None when Map.isEmpty state.Members -> Some clientActorId
                            | None -> None

                        match actorIdOpt with
                        | None -> return forbidden
                        | Some actorId ->

                        let stamped = stampActor actorId event

                        // 5. Validate.
                        match Validation.validateEvent state stamped with
                        | Error errs ->
                            let detail = errs |> List.map (sprintf "%A") |> String.concat ", "
                            return unprocessable (JsonSerializer.Serialize({| error = "Validation failed"; errors = detail |}, jsonOpts))
                        | Ok validEvent ->

                            // 6. For SpaceCreated, claim the space first.
                            let (SpaceId spaceGuid) = spaceId
                            let (UserId userGuid)   = userId
                            let claimResult =
                                if (match validEvent with SpaceCreated _ -> true | _ -> false) then
                                    EventRepository.claimSpace connString spaceGuid userGuid
                                else
                                    async { return Ok () }

                            let! claimed = claimResult
                            match claimed with
                            | Error err -> return serverError err
                            | Ok () ->

                                // 7. Re-encode stamped event and write to DB.
                                let stampedJson = Serde.encodeEventJson validEvent
                                let! writeResult = EventRepository.insertEvent connString stampedJson
                                match writeResult with
                                | Error err -> return serverError err
                                | Ok ()     -> return ok ()
    }
