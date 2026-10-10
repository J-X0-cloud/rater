namespace Rater

open System
open Rater.Dsl

/// One rating characteristic of a risk, such as construction type or year built.
type AttributeDef =
    { Name: string
      Type: DataType
      Description: string option
      Required: bool
      Default: Value option
      /// For text and number attributes: the closed set of acceptable values.
      Allowed: Value list option
      Min: decimal option
      Max: decimal option
      Integer: bool }

/// Limits a coverage may be written at.
type LimitRule =
    { Min: decimal
      Max: decimal
      /// Limits must be a whole multiple of this increment, when present.
      Increment: decimal option
      Default: decimal option }

type CoverageDef =
    { Code: string
      Name: string
      Required: bool
      Limit: LimitRule
      Deductibles: decimal list
      DefaultDeductible: decimal option
      /// Coverages that must also be on the quote; `premium.X` references add to this implicitly.
      Requires: string list
      MinimumPremium: decimal
      Algorithm: Algorithm }

type UnderwritingAction =
    | Refer
    | Decline

type UnderwritingRule =
    { Id: string
      Description: string
      Condition: Expr
      Action: UnderwritingAction
      Message: string }

type Fee =
    { Code: string
      Name: string
      Amount: decimal }

type Tax =
    { Code: string
      Name: string
      Rate: decimal
      /// Whether the tax is levied on policy fees as well as premium.
      IncludesFees: bool }

type PremiumRules =
    { Rounding: RoundingMode
      /// Decimal places for coverage and policy premiums (0 rounds to whole currency units).
      Places: int
      PolicyMinimum: decimal
      /// Days in the annual basis used to pro-rate short and long terms.
      AnnualBasisDays: int
      MinimumTermDays: int
      MaximumTermDays: int }

type Product =
    { Code: string
      Name: string
      Currency: string
      Description: string option
      /// First policy effective date the product may be written for.
      Available: DateOnly
      /// Last effective date for new business, if the product has been withdrawn.
      Withdrawn: DateOnly option
      QuoteValidityDays: int
      Attributes: AttributeDef list
      Coverages: CoverageDef list
      Tables: VersionedTable list
      Rules: UnderwritingRule list
      Fees: Fee list
      Taxes: Tax list
      Premium: PremiumRules }

/// A problem with a product definition, located as precisely as the source allows.
type ProductIssue =
    { Severity: Severity
      Where: string
      Message: string }

    override i.ToString() =
        let tag =
            match i.Severity with
            | Severity.Error -> "error"
            | Severity.Warning -> "warning"

        $"{tag}: {i.Where}: {i.Message}"

/// A product that has passed validation, with algorithms compiled and coverages ordered so
/// that any coverage referring to another's premium is rated after it.
type RatingPlan =
    { Product: Product
      Order: CoverageDef list
      Compiled: Map<string, Interpreter.CompiledAlgorithm>
      Dependencies: Map<string, Set<string>>
      RuleCode: Map<string, EvalContext -> Result<Value, RuntimeError>>
      Warnings: ProductIssue list }

[<RequireQualifiedAccess>]
module Product =

    let attribute (p: Product) name =
        p.Attributes |> List.tryFind (fun a -> a.Name = name)

    let coverage (p: Product) code =
        p.Coverages |> List.tryFind (fun c -> c.Code = code)

    let table (p: Product) name =
        p.Tables |> List.tryFind (fun t -> t.Name = name)

    /// Coverage codes referenced as `premium.CODE` inside an algorithm.
    let premiumReferences (c: CoverageDef) =
        c.Algorithm.Steps
        |> List.collect (fun s -> Expr.references s.Expr)
        |> List.choose (function
            | [ "premium"; code ] -> Some code
            | _ -> None)
        |> Set.ofList

    /// Kahn's algorithm over the "rated after" relation. Ties are broken by declaration order so
    /// that worksheets list coverages the way the product author wrote them.
    let topologicalOrder (codes: string list) (deps: Map<string, Set<string>>) : Result<string list, string list> =
        let index = codes |> List.mapi (fun i c -> c, i) |> Map.ofList

        let rec go (ordered: string list) (remaining: Set<string>) =
            if remaining.IsEmpty then
                Ok(List.rev ordered)
            else
                let ready =
                    remaining
                    |> Set.filter (fun c ->
                        deps
                        |> Map.tryFind c
                        |> Option.defaultValue Set.empty
                        |> Set.forall (fun d -> not (remaining.Contains d)))

                if ready.IsEmpty then
                    Error(remaining |> Set.toList |> List.sortBy (fun c -> index[c]))
                else
                    let next = ready |> Seq.minBy (fun c -> index[c])
                    go (next :: ordered) (remaining.Remove next)

        go [] (Set.ofList codes)

    let private issue severity where message =
        { Severity = severity
          Where = where
          Message = message }

    let private err = issue Severity.Error
    let private warn = issue Severity.Warning

    let private checkAttribute (a: AttributeDef) =
        let where = $"attribute '{a.Name}'"

        [ match a.Default with
          | Some d when d.Type <> a.Type -> yield err where $"default {Value.describe d} is not a {a.Type}"
          | _ -> ()

          match a.Allowed with
          | Some allowed ->
              if allowed.IsEmpty then
                  yield err where "the allowed list is empty"

              for v in allowed do
                  if v.Type <> a.Type then
                      yield err where $"allowed value {Value.describe v} is not a {a.Type}"

              match a.Default with
              | Some d when not (List.contains d allowed) ->
                  yield err where $"default {Value.describe d} is not one of the allowed values"
              | _ -> ()
          | None -> ()

          if (a.Min.IsSome || a.Max.IsSome || a.Integer) && a.Type <> TNumber then
              yield err where "min, max and integer only apply to number attributes"

          match a.Min, a.Max with
          | Some lo, Some hi when lo > hi -> yield err where "min is greater than max"
          | _ -> ()

          if a.Required && a.Default.IsSome then
              yield warn where "a required attribute never uses its default" ]

    let private checkCoverage (allCodes: Set<string>) (c: CoverageDef) =
        let where = $"coverage '{c.Code}'"
        let l = c.Limit

        [ if c.Code = "total" then
              yield err where "'total' is reserved for the policy total in underwriting rules"

          if l.Min < 0m || l.Min > l.Max then
              yield err where $"limit range [{l.Min}, {l.Max}] is invalid"

          match l.Increment with
          | Some inc when inc <= 0m -> yield err where "limit increment must be positive"
          | _ -> ()

          match l.Default with
          | Some d when d < l.Min || d > l.Max -> yield err where "default limit is outside the limit range"
          | _ -> ()

          if c.Deductibles |> List.exists (fun d -> d < 0m) then
              yield err where "deductibles must not be negative"

          if c.Deductibles |> List.distinct |> List.length <> c.Deductibles.Length then
              yield err where "deductible options are repeated"

          match c.DefaultDeductible with
          | Some d when not c.Deductibles.IsEmpty && not (List.contains d c.Deductibles) ->
              yield err where "default deductible is not one of the deductible options"
          | _ -> ()

          if c.MinimumPremium < 0m then
              yield err where "minimum premium must not be negative"

          for r in c.Requires do
              if r = c.Code then
                  yield err where "a coverage cannot require itself"
              elif not (allCodes.Contains r) then
                  yield err where $"requires unknown coverage '{r}'" ]

    let private checkPremiumRules (p: PremiumRules) =
        [ if p.Places < -6 || p.Places > 6 then
              yield err "premium" "places must be between -6 and 6"
          if p.PolicyMinimum < 0m then
              yield err "premium" "policy minimum must not be negative"
          if p.AnnualBasisDays <= 0 then
              yield err "premium" "annual basis must be a positive number of days"
          if p.MinimumTermDays <= 0 || p.MinimumTermDays > p.MaximumTermDays then
              yield err "premium" "term range is invalid" ]

    let private duplicates where (names: string list) =
        names
        |> List.countBy id
        |> List.filter (fun (_, n) -> n > 1)
        |> List.map (fun (n, _) -> err where $"'{n}' is defined more than once")

    let private diagIssue where (d: Diagnostic) =
        issue d.Severity $"{where} at {d.Pos}" d.Message

    /// Validates a product end to end and compiles it into a rating plan. Every problem is
    /// reported, not just the first, so a product author can fix a file in one pass.
    let plan (p: Product) : Result<RatingPlan, ProductIssue list> =
        let codes = p.Coverages |> List.map (fun c -> c.Code)
        let codeSet = Set.ofList codes

        let tables =
            p.Tables |> List.map (fun t -> t.Name, VersionedTable.dimensions t) |> Map.ofList

        let attributes = p.Attributes |> List.map (fun a -> a.Name, a.Type) |> Map.ofList

        let structural =
            [ yield! duplicates "attributes" (p.Attributes |> List.map (fun a -> a.Name))
              yield! duplicates "coverages" codes
              yield! duplicates "tables" (p.Tables |> List.map (fun t -> t.Name))
              yield! duplicates "rules" (p.Rules |> List.map (fun r -> r.Id))
              yield! duplicates "fees" (p.Fees |> List.map (fun f -> f.Code))
              yield! duplicates "taxes" (p.Taxes |> List.map (fun t -> t.Code))
              yield! p.Attributes |> List.collect checkAttribute
              yield! p.Coverages |> List.collect (checkCoverage codeSet)
              yield! checkPremiumRules p.Premium

              if p.Coverages.IsEmpty then
                  yield err "product" "a product needs at least one coverage"

              if p.QuoteValidityDays <= 0 then
                  yield err "product" "quote validity must be at least one day"

              match p.Withdrawn with
              | Some w when w < p.Available -> yield err "product" "withdrawn before it was available"
              | _ -> ()

              for t in p.Tables do
                  match VersionedTable.firstEffective t with
                  | Some first when first > p.Available ->
                      yield
                          warn
                              $"table '{t.Name}'"
                              $"first version is effective {Dates.iso first}, after the product becomes available"
                  | _ -> ()

              for f in p.Fees do
                  if f.Amount < 0m then
                      yield err $"fee '{f.Code}'" "amount must not be negative"

              for t in p.Taxes do
                  if t.Rate < 0m || t.Rate > 1m then
                      yield err $"tax '{t.Code}'" "rate must be between 0 and 1" ]

        let algorithmEnv =
            { Attributes = attributes
              AllowCoverage = true
              PremiumRefs = codeSet
              Tables = tables }

        let algorithmIssues, compiled =
            p.Coverages
            |> List.fold
                (fun (issues, compiled: Map<string, Interpreter.CompiledAlgorithm>) c ->
                    let where = $"coverage '{c.Code}' algorithm"

                    match TypeCheck.algorithm algorithmEnv c.Algorithm with
                    | Error diags -> issues @ (diags |> List.map (diagIssue where)), compiled
                    | Ok checkedAlg ->
                        let warnings = checkedAlg.Warnings |> List.map (diagIssue where)

                        match Interpreter.compileAlgorithm c.Algorithm with
                        | Ok code -> issues @ warnings, compiled.Add(c.Code, code)
                        | Error msg -> issues @ warnings @ [ err where msg ], compiled)
                ([], Map.empty)

        let ruleEnv =
            { algorithmEnv with
                AllowCoverage = false
                PremiumRefs = codeSet.Add "total" }

        let ruleIssues, ruleCode =
            p.Rules
            |> List.fold
                (fun (issues, code: Map<string, _>) r ->
                    let where = $"rule '{r.Id}'"

                    match TypeCheck.expression ruleEnv r.Condition with
                    | Error diags -> issues @ (diags |> List.map (diagIssue where)), code
                    | Ok(TBool, warnings) ->
                        issues @ (warnings |> List.map (diagIssue where)),
                        code.Add(r.Id, Interpreter.compileExpression r.Condition)
                    | Ok(t, _) -> issues @ [ err where $"condition must be a bool but is a {t}" ], code)
                ([], Map.empty)

        let dependencies =
            p.Coverages
            |> List.map (fun c -> c.Code, Set.union (premiumReferences c) (Set.ofList c.Requires))
            |> Map.ofList

        let selfReferences =
            p.Coverages
            |> List.filter (fun c -> (premiumReferences c).Contains c.Code)
            |> List.map (fun c -> err $"coverage '{c.Code}'" "an algorithm cannot use its own premium")

        let ratingDeps =
            p.Coverages
            |> List.map (fun c -> c.Code, (premiumReferences c).Remove c.Code)
            |> Map.ofList

        let orderResult = topologicalOrder codes ratingDeps

        let cycleIssues =
            match orderResult with
            | Ok _ -> []
            | Error stuck ->
                let shown = stuck |> String.joinWith ", "
                [ err "coverages" $"premium references form a cycle among: {shown}" ]

        let all = structural @ algorithmIssues @ ruleIssues @ selfReferences @ cycleIssues

        match all |> List.filter (fun i -> i.Severity = Severity.Error), orderResult with
        | [], Ok order ->
            let byCode = p.Coverages |> List.map (fun c -> c.Code, c) |> Map.ofList

            Ok
                { Product = p
                  Order = order |> List.map (fun c -> byCode[c])
                  Compiled = compiled
                  Dependencies = dependencies
                  RuleCode = ruleCode
                  Warnings = all }
        | errors, _ -> Error errors
