module Rater.Cli.Program

open System
open System.IO
open Rater
open Rater.Json

let usage =
    """rater - insurance rating engine

Usage:
  rater validate <product.json>...             Check product definitions and report every problem
  rater rate <product.json> <request.json>     Rate a request and print the worksheet
             [--json] [--date yyyy-MM-dd]
  rater tables <product.json> [--date d]       List tables and the version in force on a date
  rater diff <product.json> <table> <from> <to>
                                               Compare the versions of a table in force on two dates
  rater impact <product.json> <requests.json> --from <date> --to <date>
                                               Re-rate a book of requests under two rate dates
  rater bench <product.json> <request.json> [--iterations n]
                                               Measure load, rating and lifecycle throughput
"""

exception CliError of string

let private fail msg = raise (CliError msg)

let private readFile path =
    if not (File.Exists path) then
        fail $"file not found: {path}"

    File.ReadAllText path

let private loadPlan path =
    match ProductCodec.load (readFile path) with
    | Ok plan -> plan
    | Error errs -> fail (String.joinWith "\n" (path :: errs))

let private loadRequest path =
    match Decode.fromString ModelCodec.ratingRequest (readFile path) with
    | Ok r -> r
    | Error errs -> fail (String.joinWith "\n" (path :: (errs |> List.map string)))

let private parseDate (s: string) =
    match Value.parseDate s with
    | Some d -> d
    | None -> fail $"'{s}' is not a date in yyyy-MM-dd form"

/// Splits arguments into positionals and `--name value` / `--flag` options.
let private parseOptions (flags: Set<string>) (args: string list) =
    let rec go positional options rest =
        match rest with
        | [] -> List.rev positional, options
        | (a: string) :: more when a.StartsWith "--" ->
            let name = a.Substring 2

            if flags.Contains name then
                go positional (Map.add name "true" options) more
            else
                match more with
                | v :: more -> go positional (Map.add name v options) more
                | [] -> fail $"option --{name} needs a value"
        | a :: more -> go (a :: positional) options more

    go [] Map.empty args

let private shiftTo (date: DateOnly) (r: RatingRequest) =
    let shift = Dates.daysBetween r.EffectiveDate date

    { r with
        EffectiveDate = date
        ExpirationDate = r.ExpirationDate |> Option.map (fun e -> e.AddDays shift) }

let private count (n: int) (noun: string) =
    if n = 1 then $"1 {noun}" else $"{n} {noun}s"

let validate (paths: string list) =
    if paths.IsEmpty then
        fail "validate needs at least one product file"

    let mutable failures = 0

    for path in paths do
        match ProductCodec.load (readFile path) with
        | Ok plan ->
            let p = plan.Product

            let summary =
                [ count p.Coverages.Length "coverage"
                  count p.Tables.Length "table"
                  count p.Rules.Length "rule" ]
                |> String.joinWith ", "

            printfn $"{path}: ok - {p.Code} '{p.Name}', {summary}"

            for w in plan.Warnings do
                printfn $"  {w}"
        | Error errs ->
            failures <- failures + 1
            printfn $"{path}: {errs.Length} problem(s)"

            for e in errs do
                printfn $"  {e}"

    if failures > 0 then 1 else 0

let rate (args: string list) =
    let positional, options = parseOptions (set [ "json" ]) args

    match positional with
    | [ productPath; requestPath ] ->
        let plan = loadPlan productPath
        let request = loadRequest requestPath

        let request =
            match options.TryFind "date" with
            | Some d -> shiftTo (parseDate d) request
            | None -> request

        match Rating.rate plan request with
        | Ok result ->
            if options.ContainsKey "json" then
                printfn "%s" (Encode.toString (ModelCodec.encodeResult result))
            else
                printf "%s" (Render.worksheet result)

            0
        | Error issues ->
            eprintfn "the request could not be rated:"

            for i in issues do
                eprintfn $"  {i}"

            2
    | _ -> fail "rate needs a product file and a request file"

let tables (args: string list) =
    let positional, options = parseOptions Set.empty args

    match positional with
    | [ productPath ] ->
        let plan = loadPlan productPath
        let date = options.TryFind "date" |> Option.map parseDate

        let rows =
            plan.Product.Tables
            |> List.map (fun t ->
                let dims =
                    VersionedTable.dimensions t
                    |> List.map (fun d -> d.Name)
                    |> String.joinWith " x "

                let versions =
                    t.Versions |> List.map (fun v -> Dates.iso v.Effective) |> String.joinWith ", "

                let inForce =
                    match date with
                    | Some d ->
                        VersionedTable.versionAt d t
                        |> Option.map (fun v -> Dates.iso v.Effective)
                        |> Option.defaultValue "none"
                    | None -> ""

                [ t.Name; dims; versions; inForce ])

        let header =
            [ "Table"; "Dimensions"; "Versions"; (if date.IsSome then "In force" else "") ]

        printf "%s" (Render.table header rows)
        0
    | _ -> fail "tables needs a product file"

let diff (args: string list) =
    match args with
    | [ productPath; tableName; fromDate; toDate ] ->
        let plan = loadPlan productPath

        match Product.table plan.Product tableName with
        | None -> fail $"product has no table '{tableName}'"
        | Some t ->
            match VersionedTable.diff (parseDate fromDate) (parseDate toDate) t with
            | Ok d ->
                printf "%s" (Render.diff d)
                0
            | Error e -> fail e
    | _ -> fail "diff needs a product file, a table name and two dates"

let impact (args: string list) =
    let positional, options = parseOptions Set.empty args

    match positional, options.TryFind "from", options.TryFind "to" with
    | [ productPath; requestsPath ], Some fromDate, Some toDate ->
        let plan = loadPlan productPath
        let fromDate = parseDate fromDate
        let toDate = parseDate toDate

        let requests =
            match Decode.fromString (Decode.list ModelCodec.ratingRequest) (readFile requestsPath) with
            | Ok rs -> rs
            | Error errs -> fail (errs |> List.map string |> String.joinWith "\n")

        let outcomes =
            requests
            |> List.mapi (fun i r ->
                let before = Rating.rate plan (shiftTo fromDate r)
                let after = Rating.rate plan (shiftTo toDate r)

                match before, after with
                | Ok b, Ok a -> i + 1, Some(b.Premium, a.Premium)
                | _ -> i + 1, None)

        let rows =
            outcomes
            |> List.map (fun (i, o) ->
                match o with
                | Some(b, a) ->
                    let change =
                        if b = 0m then
                            ""
                        else
                            ((a - b) / b * 100m)
                                .ToString("+0.00;-0.00;0.00", Globalization.CultureInfo.InvariantCulture)
                            + "%"

                    [ string i; Render.money b; Render.money a; change ]
                | None -> [ string i; "error"; "error"; "" ])

        printf "%s" (Render.table [ "#"; Dates.iso fromDate; Dates.iso toDate; "Change" ] rows)

        let rated = outcomes |> List.choose snd

        if not rated.IsEmpty then
            let b = rated |> List.sumBy fst
            let a = rated |> List.sumBy snd
            let pct = if b = 0m then 0m else (a - b) / b * 100m

            let shown =
                pct.ToString("+0.00;-0.00;0.00", Globalization.CultureInfo.InvariantCulture)

            printfn $"\nBook of {rated.Length} risk(s): {Render.money b} -> {Render.money a} ({shown}%%)"

        if rated.Length = requests.Length then 0 else 2
    | _ -> fail "impact needs a product file, a requests file, --from and --to"

let bench (args: string list) =
    let positional, options = parseOptions Set.empty args

    match positional with
    | [ productPath; requestPath ] ->
        let iterations =
            match options.TryFind "iterations" with
            | None -> 20000
            | Some text ->
                match Int32.TryParse text with
                | true, n when n >= 100 -> n
                | _ -> fail "--iterations must be a whole number of at least 100"

        let productJson = readFile productPath
        let plan = loadPlan productPath
        let request = loadRequest requestPath

        let loadIterations = max 50 (iterations / 100)

        let load =
            Bench.measure "load + validate + compile product" loadIterations (fun () ->
                ProductCodec.load productJson |> ignore)

        let rating =
            Bench.measure "rate one request (full worksheet)" iterations (fun () ->
                Rating.rate plan request |> ignore)

        let deps =
            { Rate = fun _ r -> Rating.rate plan r
              ValidityDays = fun _ -> plan.Product.QuoteValidityDays
              Now =
                fun () ->
                    DateTimeOffset(
                        request.EffectiveDate.AddDays(-7).ToDateTime(TimeOnly.MinValue),
                        TimeSpan.Zero
                    )
              NextPolicyNumber = fun _ -> "BENCH" }

        let mutable n = 0

        let lifecycle =
            Bench.measure "quote lifecycle: create, rate, decide" (iterations / 4) (fun () ->
                n <- n + 1
                let id = $"q{n}"

                [ Create(id, plan.Product.Code, request); Rate ]
                |> List.fold
                    (fun state cmd ->
                        match Quote.decide deps state cmd with
                        | Ok events -> events |> List.fold (fun s e -> Some(Quote.evolve s e)) state
                        | Error e -> failwith (string e))
                    None
                |> ignore)

        printf "%s" (Bench.report [ load; rating; lifecycle ])
        0
    | _ -> fail "bench needs a product file and a request file"

[<EntryPoint>]
let main argv =
    try
        match List.ofArray argv with
        | "validate" :: rest -> validate rest
        | "rate" :: rest -> rate rest
        | "tables" :: rest -> tables rest
        | "diff" :: rest -> diff rest
        | "impact" :: rest -> impact rest
        | "bench" :: rest -> bench rest
        | [ "--help" ]
        | [ "-h" ]
        | [] ->
            printf "%s" usage
            0
        | cmd :: _ ->
            eprintfn $"unknown command '{cmd}'\n"
            eprintf "%s" usage
            64
    with CliError msg ->
        eprintfn "%s" msg
        1
