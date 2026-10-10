module Rater.Tests.ProductTests

open Expecto
open FsCheck
open Rater
open Rater.Dsl
open Rater.Json
open Rater.Tests.Support
open Rater.Tests.Generators

let coverage code algorithm =
    { Code = code
      Name = code
      Required = false
      Limit =
        { Min = 0m
          Max = 1000m
          Increment = None
          Default = Some 100m }
      Deductibles = []
      DefaultDeductible = None
      Requires = []
      MinimumPremium = 0m
      Algorithm = parseAlg algorithm }

let baseProduct =
    { Code = "T"
      Name = "Test"
      Currency = "USD"
      Description = None
      Available = date "2025-01-01"
      Withdrawn = None
      QuoteValidityDays = 30
      Attributes =
        [ { Name = "size"
            Type = TNumber
            Description = None
            Required = true
            Default = None
            Allowed = None
            Min = Some 0m
            Max = None
            Integer = false } ]
      Coverages = [ coverage "A" "premium := risk.size * 2" ]
      Tables = []
      Rules = []
      Fees = []
      Taxes = []
      Premium = ProductCodec.defaultPremiumRules }

let issues p =
    Product.plan p |> expectError |> List.map (fun i -> i.Message)

[<Tests>]
let tests =
    testList
        "product validation"
        [ test "a minimal product plans" {
              let plan = Product.plan baseProduct |> expectOk
              Expect.equal (plan.Order |> List.map (fun c -> c.Code)) [ "A" ] "order"
          }

          test "coverages referring to other premiums are rated after them" {
              let p =
                  { baseProduct with
                      Coverages =
                          [ coverage "C" "premium := premium.B + premium.A"
                            coverage "B" "premium := premium.A * 0.1"
                            coverage "A" "premium := 100" ] }

              let plan = Product.plan p |> expectOk
              Expect.equal (plan.Order |> List.map (fun c -> c.Code)) [ "A"; "B"; "C" ] "dependency order"
              Expect.equal plan.Dependencies["C"] (set [ "A"; "B" ]) "dependencies recorded"
          }

          test "premium reference cycles are reported" {
              let p =
                  { baseProduct with
                      Coverages = [ coverage "A" "premium := premium.B"; coverage "B" "premium := premium.A" ] }

              Expect.exists (issues p) (fun m -> m.Contains "cycle among: A, B") "cycle"
          }

          test "a coverage may not use its own premium" {
              let p = { baseProduct with Coverages = [ coverage "A" "premium := premium.A" ] }
              Expect.exists (issues p) (fun m -> m.Contains "its own premium") "self reference"
          }

          test "every problem is reported at once" {
              let p =
                  { baseProduct with
                      Coverages =
                          [ { coverage "A" "premium := risk.sise" with
                                Requires = [ "Z" ]
                                MinimumPremium = -1m }
                            coverage "A" "premium := 1" ]
                      QuoteValidityDays = 0 }

              let msgs = issues p
              Expect.isGreaterThanOrEqual msgs.Length 5 "many issues"
              Expect.exists msgs (fun m -> m.Contains "defined more than once") "duplicate code"
              Expect.exists msgs (fun m -> m.Contains "unknown coverage 'Z'") "unknown requirement"
              Expect.exists msgs (fun m -> m.Contains "did you mean 'size'") "typo in algorithm"
              Expect.exists msgs (fun m -> m.Contains "quote validity") "validity"
          }

          test "attribute definitions are checked" {
              let bad =
                  { baseProduct.Attributes.Head with
                      Name = "kind"
                      Type = TText
                      Required = false
                      Default = Some(VText "z")
                      Allowed = Some [ VText "x"; VNumber 1m ]
                      Min = Some 1m }

              let msgs = issues { baseProduct with Attributes = baseProduct.Attributes @ [ bad ] }
              Expect.exists msgs (fun m -> m.Contains "not one of the allowed values") "default outside allowed"
              Expect.exists msgs (fun m -> m.Contains "allowed value 1 is not a text") "allowed type"
              Expect.exists msgs (fun m -> m.Contains "only apply to number attributes") "min on text"
          }

          test "underwriting conditions must be bool and may use premium.total" {
              let rule id cond =
                  { Id = id
                    Description = ""
                    Condition = parseExpr cond
                    Action = Refer
                    Message = id }

              let ok = { baseProduct with Rules = [ rule "R1" "premium.total > 100 and risk.size > 3" ] }
              Expect.isOk (Product.plan ok) "valid rule"

              let bad = { baseProduct with Rules = [ rule "R2" "risk.size + 1" ] }
              Expect.exists (issues bad) (fun m -> m.Contains "condition must be a bool") "numeric condition"
          }

          test "tables first effective after availability produce a warning" {
              let table =
                  VersionedTable.create
                      "late"
                      None
                      [ { Effective = date "2025-06-01"
                          Label = None
                          Table = textTable "late" [ "a", 1m ] } ]
                  |> expectOk

              let plan = Product.plan { baseProduct with Tables = [ table ] } |> expectOk
              Expect.exists plan.Warnings (fun w -> w.Message.Contains "after the product becomes available") "warning"
          }

          testPropertyWithConfig quickConfig "topological order respects every dependency"
          <| fun (edges: (byte * byte) list) ->
              let codes = [ for i in 0..7 -> $"C{i}" ]

              // Only edges from higher to lower indexes, so the graph is acyclic.
              let deps =
                  edges
                  |> List.map (fun (a, b) -> int a % 8, int b % 8)
                  |> List.filter (fun (a, b) -> a > b)
                  |> List.groupBy fst
                  |> List.map (fun (a, bs) -> $"C{a}", bs |> List.map (fun (_, b) -> $"C{b}") |> Set.ofList)
                  |> Map.ofList

              match Product.topologicalOrder codes deps with
              | Error _ -> false
              | Ok order ->
                  let position = order |> List.mapi (fun i c -> c, i) |> Map.ofList

                  order.Length = codes.Length
                  && deps |> Map.forall (fun c ds -> ds |> Set.forall (fun d -> position[d] < position[c]))

          test "both sample products validate without errors" {
              Expect.isEmpty (homeowners.Value.Warnings |> List.filter (fun w -> w.Severity = Severity.Error)) "HO3"
              Expect.isEmpty (auto.Value.Warnings |> List.filter (fun w -> w.Severity = Severity.Error)) "PPA"
          } ]
