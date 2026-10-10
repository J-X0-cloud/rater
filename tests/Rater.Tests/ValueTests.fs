module Rater.Tests.ValueTests

open System
open Expecto
open Rater
open Rater.Tests.Support
open Rater.Tests.Generators

[<Tests>]
let tests =
    testList
        "values and dates"
        [ test "numbers compare by value regardless of scale" {
              Expect.equal (VNumber 1.50m) (VNumber 1.5m) "equal"
              Expect.equal (hash (VNumber 1.50m)) (hash (VNumber 1.5m)) "same hash"
              Expect.equal (string (VNumber 2.500m)) "2.5" "normalised display"
          }

          test "values of different types are never equal" {
              Expect.notEqual (VText "1") (VNumber 1m) "text vs number"
              Expect.notEqual (VBool true) (VText "true") "bool vs text"
          }

          test "parseAs reads each type and rejects junk" {
              Expect.equal (Value.parseAs TNumber "1,250.5" |> expectOk) (VNumber 1250.5m) "number with separator"
              Expect.equal (Value.parseAs TBool "Yes" |> expectOk) (VBool true) "yes"
              Expect.equal (Value.parseAs TDate "2026-02-28" |> expectOk) (VDate(DateOnly(2026, 2, 28))) "date"
              Expect.isError (Value.parseAs TDate "2026-02-30") "impossible date"
              Expect.isError (Value.parseAs TNumber "twelve") "word"
              Expect.isError (Value.parseAs TBool "maybe") "not a bool"
          }

          test "a year completes on its anniversary" {
              Expect.equal (Dates.yearsBetween (date "1970-06-15") (date "2026-06-14")) 55 "day before"
              Expect.equal (Dates.yearsBetween (date "1970-06-15") (date "2026-06-15")) 56 "anniversary"
          }

          test "leap-day birthdays complete on 28 February in common years" {
              Expect.equal (Dates.yearsBetween (date "2000-02-29") (date "2001-02-28")) 1 "28 Feb"
              Expect.equal (Dates.yearsBetween (date "2000-02-29") (date "2004-02-28")) 3 "leap year: not yet"
              Expect.equal (Dates.yearsBetween (date "2000-02-29") (date "2004-02-29")) 4 "leap year anniversary"
          }

          test "months between counts whole months, clamping month-end anniversaries" {
              Expect.equal (Dates.monthsBetween (date "2026-01-31") (date "2026-02-27")) 0 "not yet"
              Expect.equal (Dates.monthsBetween (date "2026-01-31") (date "2026-02-28")) 1 "end of a short month"
              Expect.equal (Dates.monthsBetween (date "2026-01-15") (date "2026-03-15")) 2 "two months"
              Expect.equal (Dates.monthsBetween (date "2026-03-15") (date "2026-01-15")) -2 "negative"
          }

          testPropertyWithConfig config "years between is antisymmetric"
          <| fun (a: DateOnly) (b: DateOnly) -> Dates.yearsBetween a b = -(Dates.yearsBetween b a)

          testPropertyWithConfig config "adding n years gives n whole years"
          <| fun (a: DateOnly) (n: byte) ->
              let n = int n % 60
              Dates.yearsBetween a (a.AddYears n) = n

          testPropertyWithConfig config "years between never exceeds the calendar year difference"
          <| fun (a: DateOnly) (b: DateOnly) ->
              let ys = Dates.yearsBetween a b
              let calendar = b.Year - a.Year
              if b >= a then ys = calendar || ys = calendar - 1 else ys = calendar || ys = calendar + 1

          testPropertyWithConfig config "days between is additive"
          <| fun (a: DateOnly) (b: DateOnly) (c: DateOnly) ->
              Dates.daysBetween a b + Dates.daysBetween b c = Dates.daysBetween a c

          test "edit distance counts a transposition as one edit" {
              Expect.equal (String.editDistance "label" "lable") 1 "transposition"
              Expect.equal (String.editDistance "kitten" "sitting") 3 "classic"
              Expect.equal (String.editDistance "" "abc") 3 "insertions"
          }

          test "edit distance suggestions" {
              Expect.equal (String.suggest [ "territory"; "construction" ] "teritory") (Some "territory") "typo"
              Expect.equal (String.suggest [ "territory"; "construction" ] "zzz") None "nothing close"
          } ]
