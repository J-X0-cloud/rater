namespace Rater

open System
open System.Collections.Generic
open Rater.Dsl

/// An attribute value as it arrives from a request, before it is checked against the product.
type RawValue =
    | RawNumber of decimal
    | RawText of string
    | RawBool of bool
    | RawNull

type CoverageSelection =
    { Code: string
      Limit: decimal option
      Deductible: decimal option }

type RatingRequest =
    { EffectiveDate: DateOnly
      ExpirationDate: DateOnly option
      Renewal: bool
      Risk: Map<string, RawValue>
      Coverages: CoverageSelection list }

type RatingIssue =
    | NotAvailable of string
    | InvalidTerm of string
    | MissingAttribute of name: string
    | UnknownAttribute of name: string * suggestion: string option
    | InvalidAttribute of name: string * message: string
    | UnknownCoverage of code: string * suggestion: string option
    | DuplicateCoverage of code: string
    | MissingRequiredCoverage of code: string
    | MissingDependency of code: string * requires: string
    | InvalidLimit of code: string * message: string
    | InvalidDeductible of code: string * message: string
    | NoRatesInForce of table: string * date: DateOnly
    | EvaluationFailed of coverage: string * error: RuntimeError

    override i.ToString() =
        let hint =
            function
            | Some s -> $"; did you mean '{s}'?"
            | None -> ""

        match i with
        | NotAvailable m -> m
        | InvalidTerm m -> m
        | MissingAttribute n -> $"risk attribute '{n}' is required"
        | UnknownAttribute(n, s) -> $"'{n}' is not a risk attribute of this product{hint s}"
        | InvalidAttribute(n, m) -> $"risk attribute '{n}': {m}"
        | UnknownCoverage(c, s) -> $"'{c}' is not a coverage of this product{hint s}"
        | DuplicateCoverage c -> $"coverage '{c}' is selected more than once"
        | MissingRequiredCoverage c -> $"coverage '{c}' is required"
        | MissingDependency(c, r) -> $"coverage '{c}' can only be written with '{r}'"
        | InvalidLimit(c, m) -> $"coverage '{c}' limit: {m}"
        | InvalidDeductible(c, m) -> $"coverage '{c}' deductible: {m}"
        | NoRatesInForce(t, d) -> $"table '{t}' has no rates in force on {Dates.iso d}"
        | EvaluationFailed(c, e) -> $"coverage '{c}': {e}"

type CoveragePremium =
    { Code: string
      Name: string
      Limit: decimal
      Deductible: decimal
      Worksheet: Worksheet
      /// Annual premium after the coverage minimum and rounding.
      AnnualPremium: decimal
      MinimumApplied: bool
      /// Premium for the actual term, including any share of the policy minimum uplift.
      WrittenPremium: decimal }

type Charge =
    { Code: string
      Name: string
      Amount: decimal }

/// Which version of each table priced the quote; recorded so a quote can be reproduced.
type TableProvenance =
    { Table: string
      Effective: DateOnly
      Label: string option }

type RatingResult =
    { Product: string
      Currency: string
      EffectiveDate: DateOnly
      ExpirationDate: DateOnly
      TermDays: int
      TermFactor: decimal
      Tables: TableProvenance list
      Coverages: CoveragePremium list
      PolicyMinimumUplift: decimal
      Premium: decimal
      Fees: Charge list
      Taxes: Charge list
      Total: decimal
      Findings: Finding list
      Decision: Decision }

[<RequireQualifiedAccess>]
module Rating =

    let private chargePlaces = 2

    /// Fraction of an annual premium earned over a term: whole years count as one each and the
    /// remaining days are pro-rated over the annual basis. An exact one-year term is therefore
    /// 1.0 even when it spans 29 February.
    let termFactor (basisDays: int) (effective: DateOnly) (expiration: DateOnly) =
        let years = max 0 (Dates.yearsBetween effective expiration)
        let remainder = Dates.daysBetween (effective.AddYears years) expiration
        decimal years + decimal remainder / decimal basisDays

    let private coerce (a: AttributeDef) (raw: RawValue) : Result<Value, string> =
        match a.Type, raw with
        | TNumber, RawNumber n -> Ok(VNumber n)
        | TNumber, RawText s -> Value.parseAs TNumber s
        | TText, RawText s -> Ok(VText s)
        | TBool, RawBool b -> Ok(VBool b)
        | TBool, RawText s -> Value.parseAs TBool s
        | TDate, RawText s -> Value.parseAs TDate s
        | t, RawNumber n -> Error $"expected a {t} but got the number {Decimal.toInvariantString n}"
        | t, RawBool b -> Error $"""expected a {t} but got {(if b then "true" else "false")}"""
        | t, RawNull -> Error $"expected a {t} but got null"

    let private checkDomain (a: AttributeDef) (v: Value) : string list =
        [ match a.Allowed with
          | Some allowed when not (List.contains v allowed) ->
              let shown = allowed |> List.map Value.describe |> String.joinWith ", "
              yield $"{Value.describe v} is not one of {shown}"
          | _ -> ()

          match v with
          | VNumber n ->
              match a.Min with
              | Some lo when n < lo -> yield $"{Decimal.toInvariantString n} is below the minimum {Decimal.toInvariantString lo}"
              | _ -> ()

              match a.Max with
              | Some hi when n > hi -> yield $"{Decimal.toInvariantString n} is above the maximum {Decimal.toInvariantString hi}"
              | _ -> ()

              if a.Integer && n <> Math.Truncate n then
                  yield $"{Decimal.toInvariantString n} is not a whole number"
          | _ -> () ]

    /// Checks raw risk input against the product's attribute schema and fills in defaults.
    let validateRisk (p: Product) (risk: Map<string, RawValue>) : Result<Map<string, Value>, RatingIssue list> =
        let known = p.Attributes |> List.map (fun a -> a.Name) |> Set.ofList

        let unknown =
            risk
            |> Map.toList
            |> List.filter (fun (k, _) -> not (known.Contains k))
            |> List.map (fun (k, _) -> UnknownAttribute(k, String.suggest known k))

        let checkedAttrs =
            p.Attributes
            |> Result.traverseAll (fun a ->
                match risk.TryFind a.Name with
                | None
                | Some RawNull ->
                    match a.Default with
                    | Some d when not a.Required -> Ok(Some(a.Name, d))
                    | _ when a.Required -> Error [ MissingAttribute a.Name ]
                    | _ -> Ok None
                | Some raw ->
                    match coerce a raw with
                    | Error m -> Error [ InvalidAttribute(a.Name, m) ]
                    | Ok v ->
                        match checkDomain a v with
                        | [] -> Ok(Some(a.Name, v))
                        | problems -> Error(problems |> List.map (fun m -> InvalidAttribute(a.Name, m))))

        match checkedAttrs, unknown with
        | Ok values, [] -> Ok(values |> List.choose id |> Map.ofList)
        | Ok _, u -> Error u
        | Error e, u -> Error(u @ e)

    /// Resolves coverage selections to concrete limits and deductibles, applying defaults and
    /// enforcing required coverages and dependencies.
    let validateCoverages
        (plan: RatingPlan)
        (selections: CoverageSelection list)
        : Result<(CoverageDef * decimal * decimal) list, RatingIssue list> =
        let p = plan.Product
        let codes = p.Coverages |> List.map (fun c -> c.Code)
        let selected = selections |> List.map (fun s -> s.Code) |> Set.ofList

        let structural =
            [ yield!
                  selections
                  |> List.countBy (fun s -> s.Code)
                  |> List.filter (fun (_, n) -> n > 1)
                  |> List.map (fun (c, _) -> DuplicateCoverage c)

              for s in selections do
                  if not (List.contains s.Code codes) then
                      yield UnknownCoverage(s.Code, String.suggest codes s.Code)

              for c in p.Coverages do
                  if c.Required && not (selected.Contains c.Code) then
                      yield MissingRequiredCoverage c.Code

              for s in selections |> List.distinctBy (fun s -> s.Code) do
                  match plan.Dependencies.TryFind s.Code with
                  | Some deps ->
                      for d in deps do
                          if not (selected.Contains d) then
                              yield MissingDependency(s.Code, d)
                  | None -> () ]

        let resolved =
            selections
            |> List.distinctBy (fun s -> s.Code)
            |> Result.traverseAll (fun s ->
                match Product.coverage p s.Code with
                | None -> Ok None
                | Some c ->
                    let limit =
                        match s.Limit |> Option.orElse c.Limit.Default with
                        | None -> Error [ InvalidLimit(c.Code, "a limit is required") ]
                        | Some l when l < c.Limit.Min || l > c.Limit.Max ->
                            Error
                                [ InvalidLimit(
                                      c.Code,
                                      $"{Decimal.toInvariantString l} is outside {Decimal.toInvariantString c.Limit.Min} to {Decimal.toInvariantString c.Limit.Max}"
                                  ) ]
                        | Some l ->
                            match c.Limit.Increment with
                            | Some inc when (l - c.Limit.Min) % inc <> 0m ->
                                Error
                                    [ InvalidLimit(
                                          c.Code,
                                          $"{Decimal.toInvariantString l} must be in steps of {Decimal.toInvariantString inc} from {Decimal.toInvariantString c.Limit.Min}"
                                      ) ]
                            | _ -> Ok l

                    let deductible =
                        match s.Deductible |> Option.orElse c.DefaultDeductible, c.Deductibles with
                        | None, [] -> Ok 0m
                        | None, _ -> Error [ InvalidDeductible(c.Code, "a deductible must be chosen") ]
                        | Some d, [] when d = 0m -> Ok 0m
                        | Some _, [] -> Error [ InvalidDeductible(c.Code, "this coverage has no deductible") ]
                        | Some d, options when List.contains d options -> Ok d
                        | Some d, options ->
                            let shown = options |> List.map Decimal.toInvariantString |> String.joinWith ", "
                            Error [ InvalidDeductible(c.Code, $"{Decimal.toInvariantString d} is not one of {shown}") ]

                    Result.zip limit deductible |> Result.map (fun (l, d) -> Some(c, l, d)))

        match resolved, structural with
        | Ok items, [] ->
            // Rate in dependency order, not in the order the request listed them.
            let byCode = items |> List.choose id |> List.map (fun (c, l, d) -> c.Code, (c, l, d)) |> Map.ofList
            Ok(plan.Order |> List.choose (fun c -> byCode.TryFind c.Code))
        | Ok _, s -> Error s
        | Error e, s -> Error(s @ e)

    let private checkTerm (p: Product) (req: RatingRequest) =
        let expiration =
            req.ExpirationDate |> Option.defaultWith (fun () -> req.EffectiveDate.AddYears 1)

        let days = Dates.daysBetween req.EffectiveDate expiration

        [ if req.EffectiveDate < p.Available then
              yield NotAvailable $"product '{p.Code}' is not available before {Dates.iso p.Available}"

          match p.Withdrawn with
          | Some w when req.EffectiveDate > w && not req.Renewal ->
              yield NotAvailable $"product '{p.Code}' was withdrawn for new business after {Dates.iso w}"
          | _ -> ()

          if days < p.Premium.MinimumTermDays || days > p.Premium.MaximumTermDays then
              yield
                  InvalidTerm
                      $"term of {days} days is outside the allowed {p.Premium.MinimumTermDays} to {p.Premium.MaximumTermDays} days" ],
        expiration,
        days

    let private resolveTables (p: Product) (date: DateOnly) =
        p.Tables
        |> Result.traverseAll (fun t ->
            match VersionedTable.versionAt date t with
            | Some v -> Ok(t.Name, v)
            | None -> Error [ NoRatesInForce(t.Name, date) ])

    /// Rates a request against a validated plan, returning a full worksheet, premium breakdown and
    /// underwriting decision, or every reason the request cannot be rated.
    let rate (plan: RatingPlan) (req: RatingRequest) : Result<RatingResult, RatingIssue list> =
        let p = plan.Product
        let rules = p.Premium
        let termIssues, expiration, termDays = checkTerm p req

        let validated =
            Result.zip (validateRisk p req.Risk) (validateCoverages plan req.Coverages)
            |> Result.zip (resolveTables p req.EffectiveDate)

        match validated, termIssues with
        | Error e, t -> Error(t @ e)
        | Ok _, (_ :: _ as t) -> Error t
        | Ok(tableVersions, (risk, coverages)), [] ->
            let tableMap = tableVersions |> Map.ofList
            let tables name = tableMap.TryFind name |> Option.map (fun v -> v.Table)

            let policy =
                { EffectiveDate = req.EffectiveDate
                  ExpirationDate = expiration
                  Renewal = req.Renewal }

            let riskDict = risk :> IReadOnlyDictionary<string, Value>
            let premiums = Dictionary<string, decimal>()
            let factor = termFactor rules.AnnualBasisDays req.EffectiveDate expiration
            let round = Decimal.round rules.Rounding rules.Places

            let rated =
                coverages
                |> Result.traverse (fun (c, limit, deductible) ->
                    let ctx =
                        { Risk = riskDict
                          Coverage =
                            Some
                                { Code = c.Code
                                  Limit = limit
                                  Deductible = deductible }
                          Policy = policy
                          Premiums = premiums
                          Tables = tables }

                    match Interpreter.run ctx plan.Compiled[c.Code] with
                    | Error e -> Error [ EvaluationFailed(c.Code, e) ]
                    | Ok sheet ->
                        let minimumApplied = sheet.Premium < c.MinimumPremium
                        let annual = round (max sheet.Premium c.MinimumPremium)
                        premiums[c.Code] <- annual

                        Ok
                            { Code = c.Code
                              Name = c.Name
                              Limit = limit
                              Deductible = deductible
                              Worksheet = sheet
                              AnnualPremium = annual
                              MinimumApplied = minimumApplied
                              WrittenPremium = round (annual * factor) })

            match rated with
            | Error e -> Error e
            | Ok rated ->
                let written = rated |> List.sumBy (fun c -> c.WrittenPremium)
                let uplift = max 0m (rules.PolicyMinimum - written)

                let rated =
                    if uplift = 0m then
                        rated
                    else
                        let shares =
                            Money.allocate (max 0 rules.Places) uplift (rated |> List.map (fun c -> c.WrittenPremium))

                        List.map2 (fun c share -> { c with WrittenPremium = c.WrittenPremium + share }) rated shares

                let premium = rated |> List.sumBy (fun c -> c.WrittenPremium)

                let fees =
                    p.Fees
                    |> List.map (fun f ->
                        { Code = f.Code
                          Name = f.Name
                          Amount = Decimal.round HalfUp chargePlaces f.Amount })

                let feeTotal = fees |> List.sumBy (fun f -> f.Amount)

                let taxes =
                    p.Taxes
                    |> List.map (fun t ->
                        let basis = if t.IncludesFees then premium + feeTotal else premium

                        { Code = t.Code
                          Name = t.Name
                          Amount = Decimal.round HalfUp chargePlaces (basis * t.Rate) })

                let total = premium + feeTotal + (taxes |> List.sumBy (fun t -> t.Amount))

                // Underwriting sees every coverage's premium; coverages not on the quote count as zero.
                let uwPremiums = Dictionary<string, decimal>()

                for c in p.Coverages do
                    uwPremiums[c.Code] <- 0m

                for c in rated do
                    uwPremiums[c.Code] <- c.WrittenPremium

                uwPremiums["total"] <- premium

                let findings =
                    Underwriting.evaluate
                        plan
                        { Risk = riskDict
                          Coverage = None
                          Policy = policy
                          Premiums = uwPremiums
                          Tables = tables }

                Ok
                    { Product = p.Code
                      Currency = p.Currency
                      EffectiveDate = req.EffectiveDate
                      ExpirationDate = expiration
                      TermDays = termDays
                      TermFactor = factor
                      Tables =
                        tableVersions
                        |> List.map (fun (name, v) ->
                            { Table = name
                              Effective = v.Effective
                              Label = v.Label })
                      Coverages = rated
                      PolicyMinimumUplift = uplift
                      Premium = premium
                      Fees = fees
                      Taxes = taxes
                      Total = total
                      Findings = findings
                      Decision = Underwriting.decide findings }
