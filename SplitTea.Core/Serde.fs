module SplitTea.Core.Serde

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

open SplitTea.Core

let private encGuid (g: System.Guid) = Encode.string (string g)
let private decGuid = Decode.string |> Decode.map System.Guid.Parse

let private encDateOnly (d: System.DateOnly) =
    Encode.string $"%04d{d.Year}-%02d{d.Month}-%02d{d.Day}"

let private decDateOnly =
    Decode.string |> Decode.map (fun s ->
        let p = s.Split('-')
        System.DateOnly(int p[0], int p[1], int p[2]))

let private extraCoders =
    Extra.empty
    |> Extra.withInt64
    |> Extra.withDecimal
    |> Extra.withCustom encGuid decGuid
    |> Extra.withCustom encDateOnly decDateOnly

let encodeEventJson (event: SpaceEvent) : string =
    Encode.Auto.toString(event, extra = extraCoders)

let decodeEventJson (json: string) : Result<SpaceEvent, string> =
    Decode.Auto.fromString(json, extra = extraCoders)
