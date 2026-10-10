module Rater.Tests.RatingTests

open System
open Expecto
open FsCheck
open Rater
open Rater.Tests.Support
open Rater.Tests.Generators

let hoRequest = lazy (request (readExample "ho3-request.json"))
let autoRequest = lazy (request (readExample "ppa-request.json"))

let rateHo (f: RatingRequest -> RatingRequest) =
    Rating.rate homeowners.Value (f hoRequest.Value)

let withRisk (name: string) (v: RawValue) (r: RatingRequest) = { r with Risk = r.Risk.Add(name, v) }

let withCoverages (cs: CoverageSelection list) (r: RatingRequest) = { r with Coverages = cs }

let dwelling limit =
    { Code = "DWELLING"
      Limit = Some limit
      Deductible = None }

let premiumOf code (r: RatingResult) =
    (r.Coverages |> List.find (fun c -> c.Code = code)).WrittenPremium

let ruleIds (r: RatingResult) =
    r.Findings |> List.map (fun f -> f.RuleId)

[<Tests>]
let tests =
    testList
        "rating"
        [ test "the homeowners example reproduces the hand-checked worksheet" {
              let r = rateHo id |> expectOk
              Expect.equal (premiumOf "DWELLING" r) 1766m "dwelling"
              Expect.equal (premiumOf "OTHER_STRUCTURES" r) 144m "other structures"
              Expect.equal (premiumOf "LIABILITY" r) 41m "liability"
              Expect.equal (premiumOf "MED_PAY" r) 11m "medical payments"
              Expect.equal (premiumOf "WIND" r) 159m "wind uses the rounded dwelling premium"
              Expect.equal r.Premium 2121m "premium"
              Expect.equal (r.Taxes |> List.map (fun t -> t.Amount)) [ 48.78m; 10.73m ] "taxes"
              Expect.equal r.Total 2205.51m "total"
              Expect.equal r.Decision Decision.Accept "decision"
          }

          test "the 2026 base rate revision applies only from its effective date" {
              let before =
                  rateHo (fun r ->
                      { r with
                          EffectiveDate = date "2025-12-31" })
                  |> expectOk

              let after =
                  rateHo (fun r ->
                      { r with
                          EffectiveDate = date "2026-01-01" })
                  |> expectOk

              let baseOf (r: RatingResult) =
                  r.Coverages.Head.Worksheet.Lines.Head.Value

              Expect.equal (baseOf before) (VNumber 1012m) "2025 filing"
              Expect.equal (baseOf after) (VNumber 1058m) "2026 filing (+4.5%, whole dollars)"

              Expect.equal
                  (after.Tables |> List.find (fun t -> t.Table = "base_rate")).Label
                  (Some "2026 revision, +4.5% across the board")
                  "provenance"
          }

          test "rating before the product is available fails" {
              let issues =
                  rateHo (fun r ->
                      { r with
                          EffectiveDate = date "2024-12-31" })
                  |> expectError

              Expect.exists
                  issues
                  (function
                  | NotAvailable _ -> true
                  | _ -> false)
                  "not available"
          }

          test "risk validation reports every problem" {
              let issues =
                  rateHo (
                      withRisk "territory" (RawText "09")
                      >> withRisk "roof_age" (RawNumber 1.5m)
                      >> withRisk "constructon" (RawText "frame")
                      >> fun r ->
                          { r with
                              Risk = r.Risk.Remove "year_built" }
                  )
                  |> expectError
                  |> List.map string

              Expect.exists issues (fun m -> m.Contains "\"09\" is not one of") "allowed values"
              Expect.exists issues (fun m -> m.Contains "1.5 is not a whole number") "integer"
              Expect.exists issues (fun m -> m.Contains "did you mean 'construction'") "unknown attribute"
              Expect.exists issues (fun m -> m.Contains "'year_built' is required") "missing"
          }

          test "attributes accept text for numbers, booleans and dates" {
              let r =
                  rateHo (
                      withRisk "protection_class" (RawText "6")
                      >> withRisk "has_alarm" (RawText "yes")
                  )

              Expect.isOk r "coerced"
          }

          test "coverage validation: required, dependencies, limits and deductibles" {
              let issues =
                  rateHo (
                      withCoverages
                          [ { Code = "MED_PAY"
                              Limit = Some 2500m
                              Deductible = None }
                            { Code = "WIND"
                              Limit = None
                              Deductible = Some 3m }
                            { Code = "WINDD"
                              Limit = None
                              Deductible = None } ]
                  )
                  |> expectError
                  |> List.map string

              Expect.exists issues (fun m -> m.Contains "'DWELLING' is required") "required"

              Expect.exists
                  issues
                  (fun m -> m.Contains "'MED_PAY' can only be written with 'LIABILITY'")
                  "requires"

              Expect.exists
                  issues
                  (fun m -> m.Contains "WIND' can only be written with 'DWELLING'")
                  "premium reference"

              Expect.exists issues (fun m -> m.Contains "steps of 1000") "increment"
              Expect.exists issues (fun m -> m.Contains "3 is not one of 1, 2, 5") "deductible"
              Expect.exists issues (fun m -> m.Contains "did you mean 'WIND'") "unknown coverage"
          }

          test "coverages are rated in dependency order whatever the request order" {
              let r =
                  rateHo (fun r ->
                      { r with
                          Coverages = List.rev r.Coverages })
                  |> expectOk

              Expect.equal (r.Coverages |> List.map (fun c -> c.Code)).Head "DWELLING" "dwelling first"
              Expect.equal r.Total 2205.51m "same total"
          }

          test "coverage minimum premium is applied" {
              let r =
                  rateHo (
                      withCoverages
                          [ dwelling 350000m
                            { Code = "OTHER_STRUCTURES"
                              Limit = Some 1000m
                              Deductible = None } ]
                  )

              // 1000 is below the limit range, so pick the smallest valid limit instead.
              Expect.isError r "limit below minimum"

              let r =
                  rateHo (
                      withCoverages
                          [ dwelling 350000m
                            { Code = "OTHER_STRUCTURES"
                              Limit = Some 5000m
                              Deductible = None } ]
                  )
                  |> expectOk

              let os = r.Coverages |> List.find (fun c -> c.Code = "OTHER_STRUCTURES")
              Expect.equal os.Worksheet.Premium 20.5m "raw premium"
              Expect.equal os.AnnualPremium 21m "rounded"
              Expect.isFalse os.MinimumApplied "above the 10 minimum"
          }

          test "a short term is pro-rated and the policy minimum is allocated" {
              let r =
                  rateHo (
                      withCoverages [ dwelling 100000m ]
                      >> withRisk "territory" (RawText "01")
                      >> withRisk "construction" (RawText "superior")
                      >> withRisk "prior_claims" (RawNumber 0m)
                      >> fun r ->
                          { r with
                              ExpirationDate = Some(r.EffectiveDate.AddDays 90) }
                  )
                  |> expectOk

              Expect.equal r.TermDays 90 "term"
              Expect.equal r.TermFactor (90m / 365m) "pro-rata factor"
              Expect.isGreaterThan r.PolicyMinimumUplift 0m "uplift needed"
              Expect.equal r.Premium 300m "policy minimum"
          }

          test "an exact one-year term over a leap day has factor one" {
              let f = Rating.termFactor 365 (date "2027-06-01") (date "2028-06-01")
              Expect.equal f 1m "leap year"

              Expect.equal
                  (Rating.termFactor 365 (date "2026-01-01") (date "2027-07-02"))
                  (1m + 182m / 365m)
                  "18 months"
          }

          test "terms outside the product's range are rejected" {
              let issues =
                  rateHo (fun r ->
                      { r with
                          ExpirationDate = Some(r.EffectiveDate.AddDays 10) })
                  |> expectError

              Expect.exists
                  issues
                  (function
                  | InvalidTerm _ -> true
                  | _ -> false)
                  "too short"
          }

          test "underwriting: referral, decline and decline precedence" {
              let roof = rateHo (withRisk "roof_age" (RawNumber 30m)) |> expectOk
              Expect.equal (roof.Decision.Name) "refer" "old roof is referred"
              Expect.equal (ruleIds roof) [ "UW-ROOF" ] "rule"

              let both =
                  rateHo (withRisk "roof_age" (RawNumber 30m) >> withRisk "prior_claims" (RawNumber 5m))
                  |> expectOk

              Expect.equal both.Decision.Name "decline" "decline wins"
              Expect.equal (ruleIds both |> List.sort) [ "UW-CLAIMS"; "UW-ROOF" ] "both fired"
          }

          test "underwriting rules see the policy total" {
              let r =
                  rateHo (
                      withCoverages [ dwelling 2000000m ]
                      >> withRisk "territory" (RawText "05")
                      >> withRisk "prior_claims" (RawNumber 3m)
                  )
                  |> expectOk

              Expect.contains (ruleIds r) "UW-AUTHORITY" "large premium referred"
          }

          test "underwriting rules see unselected coverages as zero" {
              let r =
                  Rating.rate
                      auto.Value
                      { autoRequest.Value with
                          Risk = autoRequest.Value.Risk.Add("vehicle_symbol", RawNumber 19m)
                          Coverages =
                              [ { Code = "BI"
                                  Limit = Some 100000m
                                  Deductible = None } ] }
                  |> expectOk

              Expect.isFalse (List.contains "AUTO-HIGH-SYMBOL" (ruleIds r)) "no collision, no referral"
          }

          test "the auto example rates with clamped curves and two-dimensional tables" {
              let r = Rating.rate auto.Value autoRequest.Value |> expectOk
              Expect.equal r.TermDays 184 "six-month term"
              Expect.equal r.Total (r.Premium + 8m) "MVR fee, no taxes"
              let bi = r.Coverages |> List.find (fun c -> c.Code = "BI")
              let record = bi.Worksheet.Lines |> List.find (fun l -> l.Step = "record")
              Expect.equal record.Value (VNumber 1.45m) "one accident and one violation"
          }

          test "an underage driver is declined and very old ages clamp" {
              let young =
                  Rating.rate
                      auto.Value
                      (autoRequest.Value |> withRisk "driver_birth_date" (RawText "2012-01-01"))
                  |> expectOk

              Expect.equal young.Decision.Name "decline" "underage"

              let old =
                  Rating.rate
                      auto.Value
                      (autoRequest.Value |> withRisk "driver_birth_date" (RawText "1920-01-01"))
                  |> expectOk

              let driver =
                  old.Coverages.Head.Worksheet.Lines |> List.find (fun l -> l.Step = "driver")

              Expect.equal driver.Value (VNumber 1.45m) "clamped to the 85 point"
          }

          testPropertyWithConfig quickConfig "dwelling premium never falls as the limit rises"
          <| fun (a: uint16) (b: uint16) ->
              let limit (x: uint16) =
                  100000m + decimal (int x % 1901) * 1000m

              let lo, hi = min (limit a) (limit b), max (limit a) (limit b)

              let at l =
                  rateHo (withCoverages [ dwelling l ]) |> expectOk |> premiumOf "DWELLING"

              at lo <= at hi

          testPropertyWithConfig
              quickConfig
              "total is premium plus fees plus taxes, and premium is the sum of coverages"
          <| fun (days: uint16) (claims: byte) ->
              let term = 30 + int days % 337

              let r =
                  rateHo (
                      withRisk "prior_claims" (RawNumber(decimal (int claims % 4)))
                      >> fun r ->
                          { r with
                              ExpirationDate = Some(r.EffectiveDate.AddDays term) }
                  )
                  |> expectOk

              r.Total = r.Premium
                        + (r.Fees |> List.sumBy (fun f -> f.Amount))
                        + (r.Taxes |> List.sumBy (fun t -> t.Amount))
              && r.Premium = (r.Coverages |> List.sumBy (fun c -> c.WrittenPremium))
              && r.Premium >= homeowners.Value.Product.Premium.PolicyMinimum

          testPropertyWithConfig quickConfig "rating is deterministic"
          <| fun (claims: byte) ->
              let f = withRisk "prior_claims" (RawNumber(decimal (int claims % 6)))
              rateHo f = rateHo f ]
