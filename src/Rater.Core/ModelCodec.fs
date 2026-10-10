namespace Rater.Json

open System.Text.Json
open System.Text.Json.Nodes
open Rater
open Rater.Dsl

/// JSON shapes for requests, rating results, quotes and product summaries.
[<RequireQualifiedAccess>]
module ModelCodec =

    // ---- decoding -------------------------------------------------------------------------

    let rawValue: Decoder<RawValue> =
        fun path e ->
            match e.ValueKind with
            | JsonValueKind.Number -> Decode.decimal path e |> Result.map RawNumber
            | JsonValueKind.String -> Ok(RawText(e.GetString()))
            | JsonValueKind.True -> Ok(RawBool true)
            | JsonValueKind.False -> Ok(RawBool false)
            | JsonValueKind.Null -> Ok RawNull
            | _ ->
                Error
                    [ { Path = path
                        Message = "a risk attribute must be a number, string, boolean or null" } ]

    let coverageSelection: Decoder<CoverageSelection> =
        Decode.strictObject
            [ "code"; "limit"; "deductible" ]
            (decoder {
                let! code = Decode.field "code" Decode.string
                and! limit = Decode.optional "limit" Decode.decimal
                and! deductible = Decode.optional "deductible" Decode.decimal

                return
                    { Code = code
                      Limit = limit
                      Deductible = deductible }
            })

    let ratingRequest: Decoder<RatingRequest> =
        Decode.strictObject
            [ "effectiveDate"; "expirationDate"; "renewal"; "risk"; "coverages" ]
            (decoder {
                let! effective = Decode.field "effectiveDate" Decode.date
                and! expiration = Decode.optional "expirationDate" Decode.date
                and! renewal = Decode.withDefault false "renewal" Decode.bool
                and! risk = Decode.field "risk" (Decode.keyValuePairs rawValue)
                and! coverages = Decode.field "coverages" (Decode.list coverageSelection)

                return
                    { EffectiveDate = effective
                      ExpirationDate = expiration
                      Renewal = renewal
                      Risk = Map.ofList risk
                      Coverages = coverages }
            })

    // ---- encoding -------------------------------------------------------------------------

    let private raw =
        function
        | RawNumber n -> Encode.decimal n
        | RawText s -> Encode.string s
        | RawBool b -> Encode.bool b
        | RawNull -> Encode.nil

    let encodeRequest (r: RatingRequest) =
        Encode.object
            [ "effectiveDate", Encode.date r.EffectiveDate
              "expirationDate", Encode.option Encode.date r.ExpirationDate
              "renewal", Encode.bool r.Renewal
              "risk", Encode.object (r.Risk |> Map.toList |> List.map (fun (k, v) -> k, raw v))
              "coverages",
              r.Coverages
              |> List.map (fun c ->
                  Encode.object
                      [ "code", Encode.string c.Code
                        "limit", Encode.option Encode.decimal c.Limit
                        "deductible", Encode.option Encode.decimal c.Deductible ])
              |> Encode.list ]

    let encodeLookup (t: LookupTrace) =
        Encode.object
            [ "table", Encode.string t.Table
              "arguments", Encode.object (t.Arguments |> List.map (fun (n, v) -> n, Encode.value v))
              "matched", t.Explanation |> List.map Encode.string |> Encode.list
              "result", Encode.decimal t.Result ]

    let encodeWorksheetLine (l: WorksheetLine) =
        Encode.object
            [ "step", Encode.string l.Step
              "formula", Encode.string l.Formula
              "value", Encode.value l.Value
              "lookups", l.Lookups |> List.map encodeLookup |> Encode.list ]

    let private actionName =
        function
        | UnderwritingAction.Refer -> "refer"
        | UnderwritingAction.Decline -> "decline"

    let encodeFinding (f: Finding) =
        Encode.object
            [ "rule", Encode.string f.RuleId
              "action", Encode.string (actionName f.Action)
              "message", Encode.string f.Message ]

    let private charge (c: Charge) =
        Encode.object
            [ "code", Encode.string c.Code
              "name", Encode.string c.Name
              "amount", Encode.decimal c.Amount ]

    let encodeResult (r: RatingResult) =
        Encode.object
            [ "product", Encode.string r.Product
              "currency", Encode.string r.Currency
              "effectiveDate", Encode.date r.EffectiveDate
              "expirationDate", Encode.date r.ExpirationDate
              "termDays", Encode.int r.TermDays
              "termFactor", Encode.decimal (Decimal.round HalfUp 6 r.TermFactor)
              "decision", Encode.string r.Decision.Name
              "findings", r.Findings |> List.map encodeFinding |> Encode.list
              "coverages",
              r.Coverages
              |> List.map (fun c ->
                  Encode.object
                      [ "code", Encode.string c.Code
                        "name", Encode.string c.Name
                        "limit", Encode.decimal c.Limit
                        "deductible", Encode.decimal c.Deductible
                        "annualPremium", Encode.decimal c.AnnualPremium
                        "minimumApplied", Encode.bool c.MinimumApplied
                        "writtenPremium", Encode.decimal c.WrittenPremium
                        "worksheet", c.Worksheet.Lines |> List.map encodeWorksheetLine |> Encode.list ])
              |> Encode.list
              "policyMinimumUplift", Encode.decimal r.PolicyMinimumUplift
              "premium", Encode.decimal r.Premium
              "fees", r.Fees |> List.map charge |> Encode.list
              "taxes", r.Taxes |> List.map charge |> Encode.list
              "total", Encode.decimal r.Total
              "tables",
              r.Tables
              |> List.map (fun t ->
                  Encode.object
                      [ "table", Encode.string t.Table
                        "effective", Encode.date t.Effective
                        "label", Encode.option Encode.string t.Label ])
              |> Encode.list ]

    let encodeQuote (q: Quote) =
        Encode.object
            [ "id", Encode.string q.Id
              "product", Encode.string q.Product
              "version", Encode.int q.Version
              "status", Encode.string q.Status.Name
              "declinedBy",
              (match q.Status with
               | Declined ByRules -> Encode.string "rules"
               | Declined(ByUnderwriter u) -> Encode.string u
               | _ -> Encode.nil)
              "declineReason", Encode.option Encode.string q.DeclineReason
              "validUntil", Encode.option Encode.date q.ValidUntil
              "policyNumber", Encode.option Encode.string q.PolicyNumber
              "openReferrals", q.OpenReferrals |> List.map encodeFinding |> Encode.list
              "approvals",
              q.Approvals
              |> List.map (fun a ->
                  Encode.object
                      [ "rule", Encode.string a.RuleId
                        "underwriter", Encode.string a.Underwriter
                        "note", Encode.string a.Note
                        "at", Encode.dateTime a.At ])
              |> Encode.list
              "request", encodeRequest q.Request
              "rating", Encode.option encodeResult q.Rating
              "createdAt", Encode.dateTime q.CreatedAt
              "updatedAt", Encode.dateTime q.UpdatedAt ]

    let encodeEvent (e: QuoteEvent) =
        let details =
            match e with
            | Created(id, product, _, _) -> [ "id", Encode.string id; "product", Encode.string product ]
            | Amended _ -> []
            | Rated(result, validUntil, _) ->
                [ "decision", Encode.string result.Decision.Name
                  "total", Encode.decimal result.Total
                  "validUntil", Encode.date validUntil ]
            | ReferralApproved a ->
                [ "rule", Encode.string a.RuleId
                  "underwriter", Encode.string a.Underwriter
                  "note", Encode.string a.Note ]
            | DeclinedByUnderwriter(u, reason, _) ->
                [ "underwriter", Encode.string u; "reason", Encode.string reason ]
            | PolicyBound(number, _) -> [ "policyNumber", Encode.string number ]
            | QuoteExpired _ -> []

        Encode.object ([ "type", Encode.string e.Name; "at", Encode.dateTime e.At ] @ details)

    let private dimensionKind =
        function
        | Exact t -> $"exact:{t}"
        | Banded -> "band"
        | Interpolated Clamp -> "interpolate:clamp"
        | Interpolated Linear -> "interpolate:linear"
        | Interpolated Reject -> "interpolate:reject"

    let encodeTableVersion (v: TableVersion) =
        Encode.object
            [ "effective", Encode.date v.Effective
              "label", Encode.option Encode.string v.Label
              "rows",
              Table.rows v.Table
              |> List.map (fun r ->
                  Encode.list ((r.Keys |> List.map (string >> Encode.string)) @ [ Encode.decimal r.Value ]))
              |> Encode.list ]

    let encodeProduct (plan: RatingPlan) =
        let p = plan.Product

        Encode.object
            [ "code", Encode.string p.Code
              "name", Encode.string p.Name
              "currency", Encode.string p.Currency
              "description", Encode.option Encode.string p.Description
              "available", Encode.date p.Available
              "withdrawn", Encode.option Encode.date p.Withdrawn
              "quoteValidityDays", Encode.int p.QuoteValidityDays
              "attributes",
              p.Attributes
              |> List.map (fun a ->
                  Encode.object
                      [ "name", Encode.string a.Name
                        "type", Encode.string (string a.Type)
                        "required", Encode.bool a.Required
                        "description", Encode.option Encode.string a.Description
                        "default", Encode.option Encode.value a.Default
                        "allowed", Encode.option (List.map Encode.value >> Encode.list) a.Allowed
                        "min", Encode.option Encode.decimal a.Min
                        "max", Encode.option Encode.decimal a.Max ])
              |> Encode.list
              "coverages",
              plan.Order
              |> List.map (fun c ->
                  Encode.object
                      [ "code", Encode.string c.Code
                        "name", Encode.string c.Name
                        "required", Encode.bool c.Required
                        "limit",
                        Encode.object
                            [ "min", Encode.decimal c.Limit.Min
                              "max", Encode.decimal c.Limit.Max
                              "increment", Encode.option Encode.decimal c.Limit.Increment
                              "default", Encode.option Encode.decimal c.Limit.Default ]
                        "deductibles", c.Deductibles |> List.map Encode.decimal |> Encode.list
                        "requires",
                        plan.Dependencies.TryFind c.Code
                        |> Option.defaultValue Set.empty
                        |> Seq.map Encode.string
                        |> Encode.list
                        "minimumPremium", Encode.decimal c.MinimumPremium
                        "algorithm",
                        c.Algorithm.Steps |> List.map (Printer.step >> Encode.string) |> Encode.list ])
              |> Encode.list
              "tables",
              p.Tables
              |> List.map (fun t ->
                  Encode.object
                      [ "name", Encode.string t.Name
                        "description", Encode.option Encode.string t.Description
                        "dimensions",
                        VersionedTable.dimensions t
                        |> List.map (fun d ->
                            Encode.object
                                [ "name", Encode.string d.Name
                                  "kind", Encode.string (dimensionKind d.Kind) ])
                        |> Encode.list
                        "versions",
                        t.Versions
                        |> List.map (fun v ->
                            Encode.object
                                [ "effective", Encode.date v.Effective
                                  "label", Encode.option Encode.string v.Label
                                  "cells", Encode.int v.Table.CellCount ])
                        |> Encode.list ])
              |> Encode.list
              "rules",
              p.Rules
              |> List.map (fun r ->
                  Encode.object
                      [ "id", Encode.string r.Id
                        "description", Encode.string r.Description
                        "when", Encode.string (Printer.expr r.Condition)
                        "action", Encode.string (actionName r.Action)
                        "message", Encode.string r.Message ])
              |> Encode.list ]

    let encodeDiff (d: TableDiff) =
        Encode.object
            [ "table", Encode.string d.Table
              "from", Encode.date d.From
              "to", Encode.date d.To
              "unchanged", Encode.int d.Unchanged
              "range",
              (match d.Range with
               | Some(lo, hi) ->
                   Encode.object
                       [ "min", Encode.decimal (Decimal.round HalfUp 6 lo)
                         "max", Encode.decimal (Decimal.round HalfUp 6 hi) ]
               | None -> Encode.nil)
              "changed",
              d.Changed
              |> List.map (fun c ->
                  Encode.object
                      [ "keys", c.Keys |> List.map (string >> Encode.string) |> Encode.list
                        "before", Encode.option Encode.decimal c.Before
                        "after", Encode.option Encode.decimal c.After
                        "change", Encode.option (fun x -> Encode.decimal (Decimal.round HalfUp 6 x)) c.Change ])
              |> Encode.list ]

    let encodeErrors (messages: string list) =
        Encode.object [ "errors", messages |> List.map Encode.string |> Encode.list ]
