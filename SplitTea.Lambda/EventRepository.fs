module SplitTea.Lambda.EventRepository

open Npgsql
open SplitTea.Core

// Loads all events for a space ordered by sequence, replays them into SpaceState.
let loadSpaceState (connString: string) (spaceId: SpaceId) : Async<SpaceState> =
    async {
        let (SpaceId g) = spaceId
        use conn = new NpgsqlConnection(connString)
        do! conn.OpenAsync() |> Async.AwaitTask
        use cmd = conn.CreateCommand()
        // row_to_json returns snake_case column names; mapped to camelCase below for Serde.
        cmd.CommandText <-
            "SELECT row_to_json(e) FROM public.events e
             WHERE space_id = $1
             ORDER BY sequence ASC"
        cmd.Parameters.AddWithValue("$1", g) |> ignore
        use! reader2 = cmd.ExecuteReaderAsync() |> Async.AwaitTask
        let eventList = System.Collections.Generic.List<SpaceEvent>()
        while reader2.Read() do
            let rowJson = reader2.GetString(0)
            // row_to_json uses snake_case column names; Serde expects camelCase fields.
            // Map the DB column names to the Serde format.
            let mapped =
                rowJson
                    .Replace("\"space_id\"",    "\"spaceId\"")
                    .Replace("\"actor_id\"",    "\"actorId\"")
                    .Replace("\"occurred_at\"", "\"occurredAt\"")
                    .Replace("\"created_at\"",  "\"createdAt\"")
                    .Replace("\"event_type\"",  "\"eventType\"")
            match Serde.decodeEventJson mapped with
            | Ok ev  -> eventList.Add(ev)
            | Error _ -> ()
        return Reducer.replayEvents (eventList |> Seq.toList)
    }

// Inserts a single event row. Uses the same JSON format as the client (camelCase).
let insertEvent (connString: string) (eventJson: string) : Async<Result<unit, string>> =
    async {
        try
            // Parse fields from the JSON the client sent (camelCase).
            let doc = System.Text.Json.JsonDocument.Parse(eventJson)
            let root = doc.RootElement
            let get (name: string) = root.GetProperty(name).GetString()
            use conn = new NpgsqlConnection(connString)
            do! conn.OpenAsync() |> Async.AwaitTask
            use cmd = conn.CreateCommand()
            cmd.CommandText <-
                "INSERT INTO public.events (id, space_id, actor_id, occurred_at, event_type, payload)
                 VALUES ($1, $2, $3, $4, $5, $6::jsonb)"
            cmd.Parameters.AddWithValue("$1", System.Guid.Parse(get "id"))          |> ignore
            cmd.Parameters.AddWithValue("$2", System.Guid.Parse(get "spaceId"))     |> ignore
            cmd.Parameters.AddWithValue("$3", System.Guid.Parse(get "actorId"))     |> ignore
            cmd.Parameters.AddWithValue("$4",
                System.DateTimeOffset.Parse(get "occurredAt"))                       |> ignore
            cmd.Parameters.AddWithValue("$5", get "eventType")                      |> ignore
            let payloadElem = root.GetProperty("payload")
            let payloadJson = payloadElem.GetRawText()
            cmd.Parameters.AddWithValue("$6", NpgsqlTypes.NpgsqlDbType.Jsonb, payloadJson) |> ignore
            do! cmd.ExecuteNonQueryAsync() |> Async.AwaitTask |> Async.Ignore
            return Ok ()
        with ex ->
            return Error ex.Message
    }

// Inserts (space_id, user_id) into space_access.
// Called before inserting the first SpaceCreated event for a new space.
let claimSpace (connString: string) (spaceId: System.Guid) (userId: System.Guid) : Async<Result<unit, string>> =
    async {
        try
            use conn = new NpgsqlConnection(connString)
            do! conn.OpenAsync() |> Async.AwaitTask
            // Verify the space has no existing events (prevent claiming an existing space).
            use checkCmd = conn.CreateCommand()
            checkCmd.CommandText <- "SELECT COUNT(*) FROM public.events WHERE space_id = $1"
            checkCmd.Parameters.AddWithValue("$1", spaceId) |> ignore
            let! count = checkCmd.ExecuteScalarAsync() |> Async.AwaitTask
            if (count :?> int64) > 0L then
                return Error "Space already exists"
            else
                use insertCmd = conn.CreateCommand()
                insertCmd.CommandText <-
                    "INSERT INTO public.space_access (space_id, user_id)
                     VALUES ($1, $2)
                     ON CONFLICT DO NOTHING"
                insertCmd.Parameters.AddWithValue("$1", spaceId) |> ignore
                insertCmd.Parameters.AddWithValue("$2", userId)   |> ignore
                do! insertCmd.ExecuteNonQueryAsync() |> Async.AwaitTask |> Async.Ignore
                return Ok ()
        with ex ->
            return Error ex.Message
    }
