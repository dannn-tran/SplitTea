module SplitTea.Lambda.JwtVerifier

open System
open System.Net.Http
open Microsoft.IdentityModel.Tokens
open System.IdentityModel.Tokens.Jwt

let private httpClient = new HttpClient()
let mutable private cachedKeys : (JsonWebKeySet * DateTime) option = None
let private cacheTtl = TimeSpan.FromMinutes 30.0

let private fetchJwks (jwksUrl: string) : Async<JsonWebKeySet> =
    async {
        let! json = httpClient.GetStringAsync(jwksUrl) |> Async.AwaitTask
        return JsonWebKeySet(json)
    }

let private getKeys (jwksUrl: string) : Async<JsonWebKeySet> =
    async {
        let now = DateTime.UtcNow
        match cachedKeys with
        | Some (keys, fetchedAt) when now - fetchedAt < cacheTtl -> return keys
        | _ ->
            let! keys = fetchJwks jwksUrl
            cachedKeys <- Some (keys, DateTime.UtcNow)
            return keys
    }

let private tryValidate (token: string) (supabaseUrl: string) (jwks: JsonWebKeySet) : string =
    let handler = JwtSecurityTokenHandler()
    let parameters = TokenValidationParameters()
    parameters.ValidateIssuerSigningKey <- true
    parameters.IssuerSigningKeys        <- jwks.Keys |> Seq.cast<SecurityKey>
    parameters.ValidateIssuer           <- true
    parameters.ValidIssuer              <- supabaseUrl + "/auth/v1"
    parameters.ValidateAudience         <- true
    parameters.ValidAudience            <- "authenticated"
    parameters.ValidateLifetime         <- true
    parameters.ClockSkew                <- TimeSpan.FromSeconds 30.0
    let principal, _ = handler.ValidateToken(token, parameters)
    let sub = principal.FindFirst("sub")
    if isNull sub then failwith "JWT has no sub claim"
    sub.Value

// Verifies a Supabase RS256 JWT and returns the user id (sub claim).
// Retries once with a fresh JWKS fetch on key-not-found (handles key rotation).
let verify (jwksUrl: string) (supabaseUrl: string) (token: string) : Async<Result<string, string>> =
    async {
        try
            let! jwks = getKeys jwksUrl
            try
                return Ok (tryValidate token supabaseUrl jwks)
            with :? SecurityTokenSignatureKeyNotFoundException ->
                cachedKeys <- None
                let! jwks2 = getKeys jwksUrl
                return Ok (tryValidate token supabaseUrl jwks2)
        with ex ->
            return Error ex.Message
    }
