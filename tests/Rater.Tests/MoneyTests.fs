module Rater.Tests.MoneyTests

open Expecto
open FsCheck
open Rater
open Rater.Tests.Generators

let roundingCases =
    [ HalfUp, 2.5m, 0, 3m
      HalfUp, -2.5m, 0, -3m
      HalfEven, 2.5m, 0, 2m
      HalfEven, 3.5m, 0, 4m
      Up, 2.01m, 0, 3m
      Up, -2.01m, 0, -3m
      Down, 2.99m, 0, 2m
      Down, -2.99m, 0, -2m
      HalfUp, 1.005m, 2, 1.01m
      Up, 12.3401m, 2, 12.35m
      Up, 12.34m, 2, 12.34m
      HalfUp, 1250m, -2, 1300m
      HalfEven, 1250m, -2, 1200m
      Up, 1201m, -2, 1300m
      Down, 1299m, -2, 1200m ]

[<Tests>]
let tests =
    testList
        "money"
        [ testList
              "rounding"
              [ for mode, input, places, expected in roundingCases ->
                    test $"{RoundingMode.name mode} {input} to {places} places is {expected}" {
                        Expect.equal (Decimal.round mode places input) expected "rounded value"
                    } ]

          testPropertyWithConfig config "rounding is idempotent"
          <| fun (d: decimal) (p: byte) ->
              let places = int p % 5
              let mode = [| HalfUp; HalfEven; Up; Down |][int p % 4]
              let once = Decimal.round mode places d
              Decimal.round mode places once = once

          testPropertyWithConfig config "rounding moves a value by less than one unit in the last place"
          <| fun (d: decimal) (p: byte) ->
              let places = int p % 5
              let mode = [| HalfUp; HalfEven; Up; Down |][int p % 4]
              abs (Decimal.round mode places d - d) < 1m / pown 10m places

          testPropertyWithConfig config "up never decreases magnitude and down never increases it"
          <| fun (d: decimal) -> abs (Decimal.round Up 2 d) >= abs d && abs (Decimal.round Down 2 d) <= abs d

          test "money arithmetic keeps the currency" {
              let a = Money.create "USD" 10.25m
              let b = Money.create "USD" 4.75m
              Expect.equal (a + b) (Money.create "USD" 15m) "sum"
              Expect.equal (a - b) (Money.create "USD" 5.5m) "difference"
              Expect.equal (string (a * 2m)) "USD 20.50" "display"
          }

          test "money refuses to mix currencies" {
              Expect.throws
                  (fun () -> Money.create "USD" 1m + Money.create "EUR" 1m |> ignore)
                  "currency mismatch"
          }

          test "allocation splits a cent remainder to the largest fractional shares" {
              let parts = Money.allocate 2 100m [ 1m; 1m; 1m ]
              Expect.equal parts [ 33.34m; 33.33m; 33.33m ] "first part takes the extra cent"
          }

          test "allocation with zero weights splits evenly" {
              Expect.equal (Money.allocate 0 10m [ 0m; 0m; 0m ]) [ 4m; 3m; 3m ] "even split"
          }

          test "allocation handles negative totals" {
              let parts = Money.allocate 2 -1m [ 1m; 2m ]
              Expect.equal (List.sum parts) -1m "sum preserved"
              Expect.equal parts [ -0.33m; -0.67m ] "proportional"
          }

          test "allocation rejects negative weights" {
              Expect.throws (fun () -> Money.allocate 2 1m [ 1m; -1m ] |> ignore) "negative weight"
          }

          testPropertyWithConfig config "allocation always sums to the rounded total"
          <| fun (total: decimal) (weights: NonEmptyArray<PositiveInt>) ->
              let ws = weights.Get |> Array.map (fun w -> decimal w.Get) |> List.ofArray
              let parts = Money.allocate 2 total ws
              List.sum parts = Decimal.round HalfUp 2 total

          testPropertyWithConfig config "each allocated part is within one cent of its exact share"
          <| fun (total: decimal) (weights: NonEmptyArray<PositiveInt>) ->
              let ws = weights.Get |> Array.map (fun w -> decimal w.Get) |> List.ofArray
              let rounded = Decimal.round HalfUp 2 total
              let parts = Money.allocate 2 total ws
              let sum = List.sum ws

              List.forall2 (fun part w -> abs (part - rounded * w / sum) < 0.01m) parts ws

          testPropertyWithConfig config "allocated parts are whole cents"
          <| fun (total: decimal) (weights: NonEmptyArray<PositiveInt>) ->
              let ws = weights.Get |> Array.map (fun w -> decimal w.Get) |> List.ofArray

              Money.allocate 2 total ws |> List.forall (fun p -> Decimal.round HalfUp 2 p = p) ]
