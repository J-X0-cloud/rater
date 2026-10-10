module Rater.Tests.InterpreterTests

open System
open Expecto
open Rater
open Rater.Dsl
open Rater.Tests.Support

let territory = textTable "territory" [ "01", 500m; "02", 650m ]
let aoi = interpolated "aoi" Reject [ 100000m, 0.8m; 300000m, 1.2m ]

let ctx =
    context
        [ "territory", VText "02"
          "year_built", VNumber 1990m
          "has_alarm", VBool true
          "birth_date", VDate(DateOnly(1960, 5, 1)) ]
        [ territory; aoi ]

let eval src =
    Interpreter.compileExpression (parseExpr src) ctx |> expectOk

let evalError src =
    Interpreter.compileExpression (parseExpr src) ctx |> expectError

let run src =
    let compiled = Interpreter.compileAlgorithm (parseAlg src) |> expectOk
    Interpreter.run ctx compiled

[<Tests>]
let tests =
    testList
        "interpreter"
        [ test "arithmetic is exact decimal arithmetic" {
              Expect.equal (eval "0.1 + 0.2") (VNumber 0.3m) "no binary floating point drift"
              Expect.equal (eval "1 / 3 * 3") (VNumber(1m / 3m * 3m)) "decimal division"
              Expect.equal (eval "-2 * 3 - -1") (VNumber -5m) "negatives"
          }

          test "references resolve against the context" {
              Expect.equal (eval "risk.territory") (VText "02") "risk"
              Expect.equal (eval "coverage.limit") (VNumber 250000m) "coverage"
              Expect.equal (eval "policy.term_days") (VNumber 365m) "policy"
              Expect.equal (eval "premium.OTHER") (VNumber 120m) "premium"
          }

          test "table lookups" {
              Expect.equal (eval "#territory[risk.territory]") (VNumber 650m) "exact"
              Expect.equal (eval "#aoi[coverage.limit]") (VNumber 1.1m) "interpolated"
          }

          test "conditionals and membership" {
              Expect.equal (eval "if risk.has_alarm then 0.95 else 1") (VNumber 0.95m) "if"
              Expect.equal (eval "if false then 1 elif false then 2 else 3") (VNumber 3m) "else"
              Expect.equal (eval "risk.territory in [\"01\", \"02\"]") (VBool true) "in"
              Expect.equal (eval "risk.territory in []") (VBool false) "empty"
          }

          test "and and or short-circuit" {
              Expect.equal (eval "false and 1 / 0 > 1") (VBool false) "and skips the right side"
              Expect.equal (eval "true or #territory[\"99\"] > 0") (VBool true) "or skips the right side"
          }

          test "comparisons across numbers, dates and text" {
              Expect.equal (eval "1.0 == 1") (VBool true) "scale-insensitive"
              Expect.equal (eval "@2026-01-01 < policy.expiration_date") (VBool true) "dates"
              Expect.equal (eval "\"a\" != \"b\"") (VBool true) "text"
          }

          test "built-in functions" {
              Expect.equal (eval "round(2.345, 2)") (VNumber 2.35m) "round"
              Expect.equal (eval "round_even(2.345, 2)") (VNumber 2.34m) "banker's"
              Expect.equal (eval "round_up(2.301)") (VNumber 3m) "up"
              Expect.equal (eval "round_down(2.999)") (VNumber 2m) "down"
              Expect.equal (eval "round(123456, -3)") (VNumber 123000m) "negative places"
              Expect.equal (eval "clamp(15, 0, 10)") (VNumber 10m) "clamp"
              Expect.equal (eval "min(3, 1, 2) + max(3, 1, 2)") (VNumber 4m) "min and max"
              Expect.equal (eval "pow(1.03, 2)") (VNumber 1.0609m) "pow"
              Expect.equal (eval "pow(2, -2)") (VNumber 0.25m) "negative pow"
              Expect.equal (eval "years_between(risk.birth_date, policy.effective_date)") (VNumber 65m) "age"
              Expect.equal (eval "months_between(@2026-01-15, @2026-04-14)") (VNumber 2m) "months"
              Expect.equal (eval "year(add_years(@2024-02-29, 1))") (VNumber 2025m) "add years"
              Expect.equal (eval "upper(lower(\"MiXeD\"))") (VText "MIXED") "case"
              Expect.equal (eval "starts_with(risk.territory, \"0\")") (VBool true) "prefix"
              Expect.equal (eval "length(\"abcd\")") (VNumber 4m) "length"
              Expect.equal (eval "floor(-1.5) + ceil(1.2) + abs(-3)") (VNumber 3m) "floor ceil abs"
          }

          test "runtime errors carry the failing node's position" {
              let e = evalError "1 + 10 / (coverage.limit - 250000)"
              Expect.stringContains e.Message "division by zero" "message"
              Expect.equal e.Pos { Line = 1; Column = 8 } "position of the division"
          }

          test "missing table rows are runtime errors" {
              let e = evalError "#territory[\"07\"]"
              Expect.stringContains e.Message "no row for key = \"07\"" "message"
          }

          test "built-in failures become runtime errors" {
              Expect.stringContains (evalError "pow(2, 0.5)").Message "whole number" "fractional power"
              Expect.stringContains (evalError "clamp(1, 5, 0)").Message "lower bound exceeds" "clamp"
              Expect.stringContains (evalError "pow(0, -1)").Message "negative power" "zero to negative"
          }

          test "overflow is caught" {
              let e = evalError "79228162514264337593543950335 * 2"
              Expect.stringContains e.Message "overflow" "overflow"
          }

          test "worksheet records each step with its lookups" {
              let sheet =
                  run "base := #territory[risk.territory]\nfactor := #aoi[coverage.limit]\npremium := round(base * factor, 2)"
                  |> expectOk

              Expect.equal sheet.Premium 715m "premium"
              Expect.equal (sheet.Lines |> List.map (fun l -> l.Step)) [ "base"; "factor"; "premium" ] "steps"
              Expect.equal sheet.Lines[0].Lookups.Head.Table "territory" "lookup recorded"
              Expect.isEmpty sheet.Lines[2].Lookups "no lookups in the last step"
              Expect.equal sheet.Lines[2].Formula "round(base * factor, 2)" "formula"
          }

          test "a failing step is named in the error" {
              let e = run "a := 1\nb := a / 0\npremium := b" |> expectError
              Expect.equal e.Step (Some "b") "step"
          }

          test "steps after premium are not evaluated" {
              let sheet = run "premium := 10\nlater := 1 / 0" |> expectOk
              Expect.equal sheet.Premium 10m "premium"
              Expect.equal sheet.Lines.Length 1 "only premium"
          }

          test "pow by squaring matches repeated multiplication" {
              for b in [ 0.5m; 1.07m; 2m; -3m ] do
                  for e in 0..12 do
                      let expected = List.replicate e b |> List.fold (*) 1m
                      Expect.equal (Builtins.power b e) expected $"{b}^{e}"
          } ]
