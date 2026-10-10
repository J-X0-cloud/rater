module Rater.Cli.Render

open System
open System.Globalization
open System.Text
open Rater

let money (d: decimal) =
    d.ToString("#,##0.00", CultureInfo.InvariantCulture)

let private pad (width: int) (s: string) =
    if s.Length >= width then s else s + String(' ', width - s.Length)

let private padLeft (width: int) (s: string) =
    if s.Length >= width then s else String(' ', width - s.Length) + s

/// Renders a plain-text table with a header rule; numeric-looking columns are right aligned.
let table (headers: string list) (rows: string list list) =
    let widths =
        headers
        |> List.mapi (fun i h ->
            rows
            |> List.map (fun r -> if i < r.Length then r[i].Length else 0)
            |> List.fold max h.Length)

    let numeric i =
        rows
        |> List.forall (fun r ->
            i >= r.Length
            || r[i] = ""
            || Decimal.TryParse(r[i].Replace(",", ""), NumberStyles.Number, CultureInfo.InvariantCulture) |> fst)

    let line (cells: string list) =
        cells
        |> List.mapi (fun i c -> if numeric i && not (rows.IsEmpty) then padLeft widths[i] c else pad widths[i] c)
        |> String.joinWith "  "

    let sb = StringBuilder()
    sb.AppendLine(headers |> List.mapi (fun i h -> pad widths[i] h) |> String.joinWith "  ") |> ignore
    sb.AppendLine(widths |> List.map (fun w -> String('-', w)) |> String.joinWith "  ") |> ignore

    for r in rows do
        sb.AppendLine(line r) |> ignore

    sb.ToString()

let worksheet (r: RatingResult) =
    let sb = StringBuilder()
    let w (s: string) = sb.AppendLine s |> ignore

    w $"Product {r.Product}, {Dates.iso r.EffectiveDate} to {Dates.iso r.ExpirationDate} ({r.TermDays} days, term factor {Decimal.toInvariantString (Decimal.round HalfUp 6 r.TermFactor)})"
    w ""

    for c in r.Coverages do
        w $"{c.Code}  {c.Name}  limit {money c.Limit}  deductible {Decimal.toInvariantString c.Deductible}"

        for line in c.Worksheet.Lines do
            w $"    {line.Step} := {line.Formula}"
            w $"        = {line.Value}"

            for lookup in line.Lookups do
                let matched = lookup.Explanation |> String.joinWith "; "
                w $"        #{lookup.Table}: {matched} -> {Decimal.toInvariantString lookup.Result}"

        let minimum = if c.MinimumApplied then " (coverage minimum applied)" else ""
        w $"    annual {money c.AnnualPremium}{minimum}, written {money c.WrittenPremium}"
        w ""

    let rows =
        [ for c in r.Coverages do
              yield [ c.Code; money c.WrittenPremium ]
          if r.PolicyMinimumUplift > 0m then
              yield [ "(policy minimum uplift included)"; money r.PolicyMinimumUplift ]
          yield [ "Premium"; money r.Premium ]
          for f in r.Fees do
              yield [ f.Name; money f.Amount ]
          for t in r.Taxes do
              yield [ t.Name; money t.Amount ]
          yield [ "Total"; money r.Total ] ]

    sb.Append(table [ "Item"; r.Currency ] rows) |> ignore
    w ""
    w $"Underwriting decision: {r.Decision.Name}"

    for f in r.Findings do
        let action =
            match f.Action with
            | UnderwritingAction.Refer -> "refer"
            | UnderwritingAction.Decline -> "decline"

        w $"    [{action}] {f.RuleId}: {f.Message}"

    w ""
    w "Rates in force:"

    for t in r.Tables do
        let label = t.Label |> Option.map (fun l -> $" ({l})") |> Option.defaultValue ""
        w $"    {t.Table}: effective {Dates.iso t.Effective}{label}"

    sb.ToString()

let diff (d: TableDiff) =
    let pct (x: decimal) =
        (x * 100m).ToString("+0.00;-0.00;0.00", CultureInfo.InvariantCulture) + "%"

    let rows =
        d.Changed
        |> List.map (fun c ->
            [ c.Keys |> List.map string |> String.joinWith ", "
              c.Before |> Option.map Decimal.toInvariantString |> Option.defaultValue "-"
              c.After |> Option.map Decimal.toInvariantString |> Option.defaultValue "-"
              c.Change |> Option.map pct |> Option.defaultValue "" ])

    let sb = StringBuilder()

    sb.AppendLine($"{d.Table}: {Dates.iso d.From} -> {Dates.iso d.To}, {d.Changed.Length} changed, {d.Unchanged} unchanged")
    |> ignore

    match d.Range with
    | Some(lo, hi) -> sb.AppendLine($"change range {pct lo} to {pct hi}") |> ignore
    | None -> ()

    if not d.Changed.IsEmpty then
        sb.Append(table [ "Keys"; "Before"; "After"; "Change" ] rows) |> ignore

    sb.ToString()
