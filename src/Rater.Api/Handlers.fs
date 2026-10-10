module Rater.Api.Handlers

open System
open System.Text.Json
open System.Text.Json.Nodes
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Giraffe
open Rater
open Rater.Json

/// Services the handlers depend on, resolved once per application.
type Env =
    { Catalog: Catalog
      Store: QuoteStore
      Clock: unit -> DateTimeOffset }

let json (status: int) (node: JsonNode) : HttpHandler =
    setStatusCode status
    >=> setHttpHeader "Content-Type" "application/json; charset=utf-8"
    >=> setBodyFromString (Encode.toCompactString node)

let errors (status: int) (messages: string list) = json status (ModelCodec.encodeErrors messages)

let private notFound what = errors 404 [ $"{what} not found" ]

let private readBody (decoder: Decoder<'a>) (next: 'a -> HttpHandler) : HttpHandler =
    fun nxt ctx ->
        task {
            let! body = ctx.ReadBodyFromRequestAsync()

            match Decode.fromString decoder body with
            | Ok value -> return! next value nxt ctx
            | Error errs -> return! errors 400 (errs |> List.map string) nxt ctx
        }

let private withPlan (code: string) (next: RatingPlan -> HttpHandler) : HttpHandler =
    fun nxt ctx ->
        let env = ctx.GetService<Env>()

        match env.Catalog.TryFind code with
        | Some plan -> next plan nxt ctx
        | None -> notFound $"product '{code}'" nxt ctx

let private dateQuery (ctx: HttpContext) (name: string) : Result<DateOnly option, string> =
    match ctx.TryGetQueryStringValue name with
    | None -> Ok None
    | Some s ->
        match Value.parseDate s with
        | Some d -> Ok(Some d)
        | None -> Error $"query parameter '{name}' must be a date in yyyy-MM-dd form"

// ---- products ---------------------------------------------------------------------------

let health: HttpHandler =
    fun next ctx ->
        let env = ctx.GetService<Env>()

        json
            200
            (Encode.object
                [ "status", Encode.string "ok"
                  "products", Encode.int env.Catalog.Plans.Count
                  "time", Encode.dateTime (env.Clock()) ])
            next
            ctx

let listProducts: HttpHandler =
    fun next ctx ->
        let env = ctx.GetService<Env>()

        let items =
            env.Catalog.Plans
            |> Map.toList
            |> List.map (fun (_, plan) ->
                let p = plan.Product

                Encode.object
                    [ "code", Encode.string p.Code
                      "name", Encode.string p.Name
                      "currency", Encode.string p.Currency
                      "available", Encode.date p.Available
                      "coverages", p.Coverages |> List.map (fun c -> Encode.string c.Code) |> Encode.list ])

        json 200 (Encode.list items) next ctx

let getProduct (code: string) : HttpHandler =
    withPlan code (fun plan -> json 200 (ModelCodec.encodeProduct plan))

let getTable (code: string, tableName: string) : HttpHandler =
    withPlan code (fun plan ->
        fun next ctx ->
            match Product.table plan.Product tableName, dateQuery ctx "date" with
            | None, _ -> notFound $"table '{tableName}'" next ctx
            | _, Error e -> errors 400 [ e ] next ctx
            | Some t, Ok date ->
                let date = date |> Option.defaultWith (fun () -> DateOnly.FromDateTime((ctx.GetService<Env>().Clock()).UtcDateTime))

                match VersionedTable.versionAt date t with
                | None -> errors 404 [ $"table '{tableName}' has no version in force on {Dates.iso date}" ] next ctx
                | Some v ->
                    let node =
                        Encode.object
                            [ "table", Encode.string t.Name
                              "dimensions",
                              VersionedTable.dimensions t |> List.map (fun d -> Encode.string d.Name) |> Encode.list
                              "version", ModelCodec.encodeTableVersion v ]

                    json 200 node next ctx)

let diffTable (code: string, tableName: string) : HttpHandler =
    withPlan code (fun plan ->
        fun next ctx ->
            match Product.table plan.Product tableName, dateQuery ctx "from", dateQuery ctx "to" with
            | None, _, _ -> notFound $"table '{tableName}'" next ctx
            | Some t, Ok(Some fromDate), Ok(Some toDate) ->
                match VersionedTable.diff fromDate toDate t with
                | Ok d -> json 200 (ModelCodec.encodeDiff d) next ctx
                | Error e -> errors 404 [ e ] next ctx
            | Some _, Error e, _
            | Some _, _, Error e -> errors 400 [ e ] next ctx
            | Some _, _, _ -> errors 400 [ "both 'from' and 'to' query parameters are required" ] next ctx)

let rate (code: string) : HttpHandler =
    withPlan code (fun plan ->
        readBody ModelCodec.ratingRequest (fun request ->
            match Rating.rate plan request with
            | Ok result -> json 200 (ModelCodec.encodeResult result)
            | Error issues -> errors 422 (issues |> List.map string)))

// ---- quotes -----------------------------------------------------------------------------

let private statusFor (e: LifecycleError) =
    match e with
    | QuoteNotFound _ -> 404
    | QuoteAlreadyExists _
    | InvalidTransition _
    | ReferralAlreadyApproved _
    | QuoteHasExpired _ -> 409
    | VersionConflict _ -> 412
    | RatingFailed _
    | UnknownReferral _
    | EffectiveDateInPast _ -> 422
    | MissingField _ -> 400

let private lifecycleError (id: string) (e: LifecycleError) =
    let e =
        match e with
        | QuoteNotFound "" -> QuoteNotFound id
        | other -> other

    let messages =
        match e with
        | RatingFailed issues -> "the quote could not be rated" :: (issues |> List.map string)
        | other -> [ string other ]

    errors (statusFor e) messages

let private quoteResponse (status: int) (record: QuoteRecord) : HttpHandler =
    setHttpHeader "ETag" $"\"{record.Quote.Version}\""
    >=> json status (ModelCodec.encodeQuote record.Quote)

/// Reads an `If-Match: "n"` header as an expected quote version.
let private expectedVersion (ctx: HttpContext) : Result<int option, string> =
    match ctx.TryGetRequestHeader "If-Match" with
    | None -> Ok None
    | Some raw ->
        match Int32.TryParse(raw.Trim().Trim('"')) with
        | true, v -> Ok(Some v)
        | _ -> Error "If-Match must hold a quote version such as \"3\""

let private execute (id: string) (command: QuoteCommand) (status: int) : HttpHandler =
    fun next ctx ->
        task {
            let env = ctx.GetService<Env>()

            match expectedVersion ctx with
            | Error e -> return! errors 400 [ e ] next ctx
            | Ok expected ->
                let! outcome =
                    match expected with
                    | Some v -> env.Store.Execute(command, id, v)
                    | None -> env.Store.Execute(command, id)

                match outcome with
                | Ok record -> return! quoteResponse status record next ctx
                | Error e -> return! lifecycleError id e next ctx
        }

type private NewQuote =
    { Id: string option
      Product: string
      Request: RatingRequest }

let private newQuote: Decoder<NewQuote> =
    decoder {
        let! id = Decode.optional "id" Decode.string
        and! product = Decode.field "product" Decode.string
        and! request = Decode.field "request" ModelCodec.ratingRequest

        return
            { Id = id
              Product = product
              Request = request }
    }
    |> Decode.strictObject [ "id"; "product"; "request" ]

let createQuote: HttpHandler =
    readBody newQuote (fun body ->
        fun next ctx ->
            let env = ctx.GetService<Env>()

            match env.Catalog.TryFind body.Product with
            | None -> errors 422 [ $"product '{body.Product}' is not offered" ] next ctx
            | Some _ ->
                let id = body.Id |> Option.defaultWith (fun () -> Guid.NewGuid().ToString("N"))

                (setHttpHeader "Location" $"/quotes/{id}"
                 >=> execute id (Create(id, body.Product, body.Request)) 201)
                    next
                    ctx)

let listQuotes: HttpHandler =
    fun next ctx ->
        task {
            let env = ctx.GetService<Env>()
            let! quotes = env.Store.List()

            let filtered =
                match ctx.TryGetQueryStringValue "status" with
                | Some s -> quotes |> List.filter (fun q -> q.Status.Name = s)
                | None -> quotes

            return! json 200 (filtered |> List.map ModelCodec.encodeQuote |> Encode.list) next ctx
        }

let private withQuote (id: string) (f: QuoteRecord -> HttpHandler) : HttpHandler =
    fun next ctx ->
        task {
            let env = ctx.GetService<Env>()
            let! record = env.Store.Get id

            match record with
            | Some r -> return! f r next ctx
            | None -> return! notFound $"quote '{id}'" next ctx
        }

let getQuote (id: string) : HttpHandler = withQuote id (quoteResponse 200)

let getEvents (id: string) : HttpHandler =
    withQuote id (fun r -> json 200 (r.Events |> List.map ModelCodec.encodeEvent |> Encode.list))

let amendQuote (id: string) : HttpHandler =
    readBody ModelCodec.ratingRequest (fun request -> execute id (Amend request) 200)

let rateQuote (id: string) : HttpHandler = execute id Rate 200

let private approval: Decoder<string * string> =
    decoder {
        let! underwriter = Decode.field "underwriter" Decode.string
        and! note = Decode.withDefault "" "note" Decode.string
        return underwriter, note
    }
    |> Decode.strictObject [ "underwriter"; "note" ]

let approveReferral (id: string, ruleId: string) : HttpHandler =
    readBody approval (fun (underwriter, note) -> execute id (ApproveReferral(ruleId, underwriter, note)) 200)

let private decline: Decoder<string * string> =
    decoder {
        let! underwriter = Decode.field "underwriter" Decode.string
        and! reason = Decode.field "reason" Decode.string
        return underwriter, reason
    }
    |> Decode.strictObject [ "underwriter"; "reason" ]

let declineQuote (id: string) : HttpHandler =
    readBody decline (fun (underwriter, reason) -> execute id (QuoteCommand.Decline(underwriter, reason)) 200)

let bindQuote (id: string) : HttpHandler = execute id Bind 200

let expireStale: HttpHandler =
    fun next ctx ->
        task {
            let env = ctx.GetService<Env>()
            let! expired = env.Store.ExpireStale()
            return! json 200 (Encode.object [ "expired", expired |> List.map Encode.string |> Encode.list ]) next ctx
        }

let unhandled (ex: Exception) (logger: Microsoft.Extensions.Logging.ILogger) : HttpHandler =
    Microsoft.Extensions.Logging.LoggerExtensions.LogError(logger, ex, "unhandled exception")
    clearResponse >=> errors 500 [ "internal error" ]
