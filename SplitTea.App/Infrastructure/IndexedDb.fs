module IndexedDb

open Fable.Core
open Fable.Core.JS
open Fable.Core.JsInterop

[<Import("openDB", "idb")>]
let private openDB (name: string) (version: int) (options: obj) : JS.Promise<obj> = jsNative

let private upgrade (db: obj) (_: int) (_: obj) (_: obj) (_: obj) : unit =
    let store : obj = db?createObjectStore("events", createObj [ "keyPath" ==> "id" ])
    store?createIndex("groupId", "groupId", createObj [ "unique" ==> false ]) |> ignore
    store?createIndex("synced",  "synced",  createObj [ "unique" ==> false ]) |> ignore

// Database handle — opened once when this module is first imported.
let private db : JS.Promise<obj> =
    openDB "splittea" 1 (createObj [ "upgrade" ==> upgrade ])

let saveEvent (event: obj) : Async<unit> =
    async {
        let! db' = db |> Async.AwaitPromise
        let! _ = (db'?put("events", event) : JS.Promise<unit>) |> Async.AwaitPromise
        ()
    }

let getEventsByGroup (groupId: string) : Async<obj[]> =
    async {
        let! db' = db |> Async.AwaitPromise
        return! (db'?getAllFromIndex("events", "groupId", groupId) : JS.Promise<obj[]>) |> Async.AwaitPromise
    }

let getPendingEvents () : Async<obj[]> =
    async {
        let! db' = db |> Async.AwaitPromise
        return! (db'?getAllFromIndex("events", "synced", false) : JS.Promise<obj[]>) |> Async.AwaitPromise
    }

let markSynced (id: string) : Async<unit> =
    async {
        let! db' = db |> Async.AwaitPromise
        let! existing = (db'?get("events", id) : JS.Promise<obj>) |> Async.AwaitPromise
        if not (isNull existing) then
            existing?synced <- true
            let! _ = (db'?put("events", existing) : JS.Promise<unit>) |> Async.AwaitPromise
            ()
    }
