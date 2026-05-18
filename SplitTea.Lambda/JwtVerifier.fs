module SplitTea.Lambda.JwtVerifier

open System
open System.Net.Http
open Microsoft.IdentityModel.Tokens
open System.IdentityModel.Tokens.Jwt

// JWKS is fetched once per Lambda warm instance and cached.
let private httpClient = new HttpClient()
let mutable private cachedKeys : JsonWebKeySet option = None

let private fetchJwks (jwksUrl: string) : Async<JsonWebKeySet> =
    async {
        let! json = httpClient.GetStringAsync(jwksUrl) |> Async.AwaitTask
        return JsonWebKeySet(json)
    }

let private getKeys (jwksUrl: string) : Async<JsonWebKeySet> =
    async {
        match cachedKeys with
        | Some keys -> return keys
        | None ->
            let! keys = fetchJwks jwksUrl
            cachedKeys <- Some keys
            return keys
    }

// Verifies a Supabase RS256 JWT and returns the user id (sub claim).
let verify (jwksUrl: string) (supabaseUrl: string) (token: string) : Async<Result<string, string>> =
    async {
        try
            let! jwks = getKeys jwksUrl
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
            if isNull sub then
                return Error "JWT has no sub claim"
            else
                return Ok sub.Value
        with ex ->
            return Error ex.Message
    }
