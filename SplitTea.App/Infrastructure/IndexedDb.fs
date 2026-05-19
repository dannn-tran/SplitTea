module IndexedDb

open Fable.Core
open Fable.Core.JS
open Fable.Core.JsInterop

[<Import("openDB", "idb")>]
let private openDB (name: string) (version: int) (options: obj) : JS.Promise<obj> = jsNative

// Schema v1: synced field holds bool | "conflicted"  (three JS types in one field)
// Schema v2: status field holds "pending" | "synced" | "conflicted"
//
// The upgrade callback migrates v1 records by mapping:
//   synced = false          → status = "pending"
//   synced = true           → status = "synced"
//   synced = "conflicted"   → status = "conflicted"
let private upgrade (db: obj) (oldVersion: int) (_newVersion: int) (tx: obj) (_: obj) : JS.Promise<unit> =
    async {
        if oldVersion < 1 then
            // Fresh install at v2: create store with the new schema directly.
            let store : obj = db?createObjectStore("events", createObj [ "keyPath" ==> "id" ])
            store?createIndex("spaceId", "spaceId", createObj [ "unique" ==> false ]) |> ignore
            store?createIndex("status",  "status",  createObj [ "unique" ==> false ]) |> ignore
        if oldVersion = 1 then
            // Migrate v1→v2: replace the "synced" index with "status" and convert values.
            let store : obj = tx?objectStore("events")
            store?deleteIndex("synced") |> ignore
            store?createIndex("status", "status", createObj [ "unique" ==> false ]) |> ignore
            let! records = (store?getAll() : JS.Promise<obj[]>) |> Async.AwaitPromise
            for record in records do
                let s = record?synced
                let status =
                    if s = box "conflicted" then "conflicted"
                    elif s = box true then "synced"
                    else "pending"
                record?status <- status
                do! (store?put(record) : JS.Promise<unit>) |> Async.AwaitPromise
    } |> Async.StartAsPromise

// Database handle — opened once when this module is first imported.
let private db : JS.Promise<obj> =
    openDB "splittea-space" 2 (createObj [ "upgrade" ==> upgrade ])

let saveEvent (event: obj) : Async<unit> =
    async {
        let! db' = db |> Async.AwaitPromise
        let! _ = (db'?put("events", event) : JS.Promise<unit>) |> Async.AwaitPromise
        ()
    }

let getEventsBySpace (spaceId: string) : Async<obj[]> =
    async {
        let! db' = db |> Async.AwaitPromise
        return! (db'?getAllFromIndex("events", "spaceId", spaceId) : JS.Promise<obj[]>) |> Async.AwaitPromise
    }

let getPendingEvents () : Async<obj[]> =
    async {
        let! db' = db |> Async.AwaitPromise
        return! (db'?getAllFromIndex("events", "status", "pending") : JS.Promise<obj[]>) |> Async.AwaitPromise
    }

let clearAllEvents () : Async<unit> =
    async {
        let! db' = db |> Async.AwaitPromise
        let! _ = (db'?clear("events") : JS.Promise<unit>) |> Async.AwaitPromise
        ()
    }

let deleteEventsBySpace (spaceId: string) : Async<unit> =
    async {
        let! db' = db |> Async.AwaitPromise
        let! records = (db'?getAllFromIndex("events", "spaceId", spaceId) : JS.Promise<obj[]>) |> Async.AwaitPromise
        for record in records do
            let! _ = (db'?delete("events", record?id) : JS.Promise<unit>) |> Async.AwaitPromise
            ()
    }

let markSynced (id: string) : Async<unit> =
    async {
        let! db' = db |> Async.AwaitPromise
        let! existing = (db'?get("events", id) : JS.Promise<obj>) |> Async.AwaitPromise
        if not (isNull existing) then
            existing?status <- "synced"
            let! _ = (db'?put("events", existing) : JS.Promise<unit>) |> Async.AwaitPromise
            ()
    }

let markConflicted (id: string) : Async<unit> =
    async {
        let! db' = db |> Async.AwaitPromise
        let! existing = (db'?get("events", id) : JS.Promise<obj>) |> Async.AwaitPromise
        if not (isNull existing) then
            existing?status <- "conflicted"
            let! _ = (db'?put("events", existing) : JS.Promise<unit>) |> Async.AwaitPromise
            ()
    }

let getConflictedEvents () : Async<obj[]> =
    async {
        let! db' = db |> Async.AwaitPromise
        return! (db'?getAllFromIndex("events", "status", "conflicted") : JS.Promise<obj[]>) |> Async.AwaitPromise
    }

let deleteEvent (id: string) : Async<unit> =
    async {
        let! db' = db |> Async.AwaitPromise
        let! _ = (db'?delete("events", id) : JS.Promise<unit>) |> Async.AwaitPromise
        ()
    }
