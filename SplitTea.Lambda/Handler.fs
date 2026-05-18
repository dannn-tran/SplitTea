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

// Overwrite the actorId in any SpaceEvent envelope with the verified actor.
let private stampActor (actorId: MemberId) (event: SpaceEvent) : SpaceEvent =
    let stamp (env: EventEnvelope<'p>) = { env with ActorId = actorId }
    match event with
    | SpaceCreated e       -> SpaceCreated       (stamp e)
    | SpaceRenamed e       -> SpaceRenamed       (stamp e)
    | MemberAdded e        -> MemberAdded        (stamp e)
    | MemberRenamed e      -> MemberRenamed      (stamp e)
    | CategoryAdded e      -> CategoryAdded      (stamp e)
    | CategoryRenamed e    -> CategoryRenamed    (stamp e)
    | CategoryArchived e   -> CategoryArchived   (stamp e)
    | ExpenseAdded e       -> ExpenseAdded       (stamp e)
    | ExpenseCorrected e   -> ExpenseCorrected   (stamp e)
    | ExpenseDeleted e     -> ExpenseDeleted     (stamp e)
    | SettlementRecorded e -> SettlementRecorded (stamp e)

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

                // 2. Parse event from request body.
                if String.IsNullOrWhiteSpace req.Body then
                    return badRequest """{"error":"Empty body"}"""
                else
                    match Serde.decodeEventJson req.Body with
                    | Error err ->
                        return badRequest (JsonSerializer.Serialize({| error = "Invalid event JSON"; detail = err |}, jsonOpts))
                    | Ok event ->

                        // 3. Determine spaceId and load current state.
                        let spaceId =
                            match event with
                            | SpaceCreated e       -> e.SpaceId
                            | SpaceRenamed e       -> e.SpaceId
                            | MemberAdded e        -> e.SpaceId
                            | MemberRenamed e      -> e.SpaceId
                            | CategoryAdded e      -> e.SpaceId
                            | CategoryRenamed e    -> e.SpaceId
                            | CategoryArchived e   -> e.SpaceId
                            | ExpenseAdded e       -> e.SpaceId
                            | ExpenseCorrected e   -> e.SpaceId
                            | ExpenseDeleted e     -> e.SpaceId
                            | SettlementRecorded e -> e.SpaceId

                        let! state = EventRepository.loadSpaceState connString spaceId

                        // 4. Resolve actorId from state.
                        //    For SpaceCreated/MemberAdded the space is empty so the member
                        //    doesn't exist yet — fall back to the client-supplied value.
                        let clientActorId =
                            match event with
                            | SpaceCreated e       -> e.ActorId
                            | SpaceRenamed e       -> e.ActorId
                            | MemberAdded e        -> e.ActorId
                            | MemberRenamed e      -> e.ActorId
                            | CategoryAdded e      -> e.ActorId
                            | CategoryRenamed e    -> e.ActorId
                            | CategoryArchived e   -> e.ActorId
                            | ExpenseAdded e       -> e.ActorId
                            | ExpenseCorrected e   -> e.ActorId
                            | ExpenseDeleted e     -> e.ActorId
                            | SettlementRecorded e -> e.ActorId

                        let actorId =
                            findMemberId state userId |> Option.defaultValue clientActorId

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
