import { serve } from "https://deno.land/std@0.224.0/http/server.ts"

const CORS_HEADERS = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Headers": "authorization, x-client-info, apikey, content-type",
}

const FRANKFURTER = "https://api.frankfurter.app"

// Currencies we care about — extend as needed.
const SUPPORTED = [
  "AUD", "CAD", "CHF", "CNY", "EUR", "GBP",
  "HKD", "JPY", "MYR", "NZD", "SGD", "USD",
]

serve(async (req: Request) => {
  if (req.method === "OPTIONS") {
    return new Response(null, { headers: CORS_HEADERS })
  }

  try {
    const url = new URL(req.url)
    const base = (url.searchParams.get("base") ?? "USD").toUpperCase()

    if (!SUPPORTED.includes(base)) {
      return new Response(
        JSON.stringify({ error: `Unsupported base currency: ${base}` }),
        { status: 400, headers: { ...CORS_HEADERS, "Content-Type": "application/json" } },
      )
    }

    const targets = SUPPORTED.filter(c => c !== base).join(",")
    const upstream = await fetch(`${FRANKFURTER}/latest?from=${base}&to=${targets}`)

    if (!upstream.ok) {
      return new Response(
        JSON.stringify({ error: "Upstream FX API unavailable" }),
        { status: 502, headers: { ...CORS_HEADERS, "Content-Type": "application/json" } },
      )
    }

    const data = await upstream.json() as { date: string; rates: Record<string, number> }

    // Add base → base identity so callers don't need to special-case it.
    const rates: Record<string, number> = { [base]: 1, ...data.rates }

    return new Response(
      JSON.stringify({ base, date: data.date, rates }),
      {
        headers: {
          ...CORS_HEADERS,
          "Content-Type": "application/json",
          // CDN-level cache: 1 hour. Deno Deploy will reuse this response.
          "Cache-Control": "public, max-age=3600, stale-while-revalidate=86400",
        },
      },
    )
  } catch (err) {
    return new Response(
      JSON.stringify({ error: String(err) }),
      { status: 500, headers: { ...CORS_HEADERS, "Content-Type": "application/json" } },
    )
  }
})
