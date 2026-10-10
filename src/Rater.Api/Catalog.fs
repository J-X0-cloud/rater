namespace Rater.Api

open System
open System.IO
open System.Threading
open Rater
open Rater.Json

/// The products this service can rate, loaded once at start-up.
type Catalog =
    { Plans: Map<string, RatingPlan> }

    member c.TryFind code = c.Plans.TryFind code

[<RequireQualifiedAccess>]
module Catalog =

    /// Loads every `*.json` file in a directory. A single invalid product fails the whole load:
    /// serving a partial catalogue would silently drop a line of business.
    let loadDirectory (dir: string) : Result<Catalog, string list> =
        if not (Directory.Exists dir) then
            Error [ $"product directory '{dir}' does not exist" ]
        else
            let files = Directory.GetFiles(dir, "*.json") |> Array.sort |> List.ofArray

            let loaded =
                files
                |> Result.traverseAll (fun path ->
                    ProductCodec.load (File.ReadAllText path)
                    |> Result.mapError (List.map (fun e -> $"{Path.GetFileName path}: {e}")))

            match loaded with
            | Error errs -> Error errs
            | Ok [] -> Error [ $"no products found in '{dir}'" ]
            | Ok plans ->
                let map, dups =
                    Map.ofListWithDuplicates (fun (p: RatingPlan) -> p.Product.Code) plans

                if dups.IsEmpty then
                    Ok { Plans = map }
                else
                    Error(
                        dups
                        |> List.map (fun d -> $"product code '{d}' is defined in more than one file")
                    )

    let ofPlans (plans: RatingPlan list) =
        { Plans = plans |> List.map (fun p -> p.Product.Code, p) |> Map.ofList }

/// Policy numbers of the form CODE-YYYY-NNNNNN, sequential per process.
type PolicyNumbers(clock: unit -> DateTimeOffset) =
    let mutable counter = 0

    member _.Next(product: string) =
        let n = Interlocked.Increment(&counter)
        $"{product}-{clock().Year}-{n:D6}"

[<RequireQualifiedAccess>]
module Lifecycle =

    let deps (catalog: Catalog) (clock: unit -> DateTimeOffset) : LifecycleDeps =
        let numbers = PolicyNumbers clock

        { Rate =
            fun product request ->
                match catalog.TryFind product with
                | Some plan -> Rating.rate plan request
                | None -> Error [ NotAvailable $"product '{product}' is not offered" ]
          ValidityDays =
            fun product ->
                catalog.TryFind product
                |> Option.map (fun p -> p.Product.QuoteValidityDays)
                |> Option.defaultValue 30
          Now = clock
          NextPolicyNumber = numbers.Next }
