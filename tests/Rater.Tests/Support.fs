module Rater.Tests.Support

open System
open System.IO
open Expecto
open Rater
open Rater.Dsl
open Rater.Json

/// The repository root, found by walking up from the test binaries to the solution file.
let repoRoot =
    let rec up (dir: DirectoryInfo) =
        if isNull dir then
            failwith "could not locate Rater.sln above the test binaries"
        elif File.Exists(Path.Combine(dir.FullName, "Rater.sln")) then
            dir.FullName
        else
            up dir.Parent

    up (DirectoryInfo AppContext.BaseDirectory)

let productPath name =
    Path.Combine(repoRoot, "products", name)

let loadPlan name =
    match ProductCodec.load (File.ReadAllText(productPath name)) with
    | Ok plan -> plan
    | Error errs -> failwithf "%s failed to load:\n%s" name (String.joinWith "\n" errs)

let homeowners = lazy (loadPlan "homeowners.json")
let auto = lazy (loadPlan "personal-auto.json")

let date (s: string) =
    match Value.parseDate s with
    | Some d -> d
    | None -> failwithf "bad date %s" s

let expectOk (r: Result<'a, 'e>) =
    match r with
    | Ok v -> v
    | Error e -> failtestf "expected Ok but got Error %A" e

let expectError (r: Result<'a, 'e>) =
    match r with
    | Ok v -> failtestf "expected Error but got Ok %A" v
    | Error e -> e

let parseExpr src = Parser.parseExpression src |> expectOk
let parseAlg src = Parser.parseAlgorithm src |> expectOk

/// Builds a one-dimensional exact table over text keys.
let textTable name (rows: (string * decimal) list) =
    Table.build
        name
        [ { Name = "key"
            Kind = Exact TText
            Keys = rows |> List.map (fun (k, _) -> Key(VText k)) |> Array.ofList } ]
        (rows |> List.map (fun (k, v) -> { Keys = [ Key(VText k) ]; Value = v }))
    |> expectOk

let interpolated name extrapolation (points: (decimal * decimal) list) =
    Table.build
        name
        [ { Name = "x"
            Kind = Interpolated extrapolation
            Keys = points |> List.map (fst >> Point) |> Array.ofList } ]
        (points |> List.map (fun (k, v) -> { Keys = [ Point k ]; Value = v }))
    |> expectOk

let policyOn (effective: string) =
    let d = date effective

    { EffectiveDate = d
      ExpirationDate = d.AddYears 1
      Renewal = false }

/// An evaluation context with the given risk values and tables.
let context (risk: (string * Value) list) (tables: Table list) : EvalContext =
    let tableMap = tables |> List.map (fun t -> t.Name, t) |> Map.ofList

    { Risk = Map.ofList risk
      Coverage =
        Some
            { Code = "TEST"
              Limit = 250000m
              Deductible = 1000m }
      Policy = policyOn "2026-01-01"
      Premiums = Map.ofList [ "OTHER", 120m ]
      Tables = fun name -> tableMap.TryFind name }

let request (json: string) =
    Decode.fromString ModelCodec.ratingRequest json |> expectOk

let readExample name =
    File.ReadAllText(Path.Combine(repoRoot, "examples", name))
