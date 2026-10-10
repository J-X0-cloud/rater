namespace Rater.Json

open System
open System.Globalization
open System.Text.Json
open Rater
open Rater.Dsl

/// Reads product definitions from JSON. Products are data: coverages, attributes, tables with
/// their filing history, underwriting rules and algorithms written in the rating language.
[<RequireQualifiedAccess>]
module ProductCodec =

    /// A decoder for a value of a known type.
    let valueOf (t: DataType) : Decoder<Value> =
        match t with
        | TNumber -> Decode.decimal |> Decode.map VNumber
        | TText -> Decode.string |> Decode.map VText
        | TBool -> Decode.bool |> Decode.map VBool
        | TDate -> Decode.date |> Decode.map VDate

    let dataType: Decoder<DataType> = Decode.string |> Decode.andThen DataType.parse

    let attribute: Decoder<AttributeDef> =
        Decode.strictObject
            [ "name"
              "type"
              "description"
              "required"
              "default"
              "allowed"
              "min"
              "max"
              "integer" ]
            (decoder {
                let! t = Decode.field "type" dataType

                return!
                    decoder {
                        let! name = Decode.field "name" Decode.string
                        and! description = Decode.optional "description" Decode.string
                        and! required = Decode.withDefault false "required" Decode.bool
                        and! dflt = Decode.optional "default" (valueOf t)
                        and! allowed = Decode.optional "allowed" (Decode.list (valueOf t))
                        and! min = Decode.optional "min" Decode.decimal
                        and! max = Decode.optional "max" Decode.decimal
                        and! integer = Decode.withDefault false "integer" Decode.bool

                        return
                            { Name = name
                              Type = t
                              Description = description
                              Required = required
                              Default = dflt
                              Allowed = allowed
                              Min = min
                              Max = max
                              Integer = integer }
                    }
            })

    let private source: Decoder<string> =
        Decode.oneOf
            [ Decode.string
              Decode.list Decode.string |> Decode.map (String.joinWith "\n") ]
            "a string or an array of lines"

    let private syntax (parse: string -> Result<'a, SyntaxError>) (d: Decoder<string>) : Decoder<'a> =
        d |> Decode.andThen (fun src -> parse src |> Result.mapError (fun e -> $"syntax error at {e}"))

    let limit: Decoder<LimitRule> =
        Decode.strictObject
            [ "min"; "max"; "increment"; "default" ]
            (decoder {
                let! min = Decode.field "min" Decode.decimal
                and! max = Decode.field "max" Decode.decimal
                and! increment = Decode.optional "increment" Decode.decimal
                and! dflt = Decode.optional "default" Decode.decimal

                return
                    { Min = min
                      Max = max
                      Increment = increment
                      Default = dflt }
            })

    let coverage: Decoder<CoverageDef> =
        Decode.strictObject
            [ "code"
              "name"
              "required"
              "limit"
              "deductibles"
              "defaultDeductible"
              "requires"
              "minimumPremium"
              "algorithm" ]
            (decoder {
                let! code = Decode.field "code" Decode.string
                and! name = Decode.field "name" Decode.string
                and! required = Decode.withDefault false "required" Decode.bool
                and! limit = Decode.field "limit" limit
                and! deductibles = Decode.withDefault [] "deductibles" (Decode.list Decode.decimal)
                and! defaultDeductible = Decode.optional "defaultDeductible" Decode.decimal
                and! requires = Decode.withDefault [] "requires" (Decode.list Decode.string)
                and! minimumPremium = Decode.withDefault 0m "minimumPremium" Decode.decimal
                and! algorithm = Decode.field "algorithm" (syntax Parser.parseAlgorithm source)

                return
                    { Code = code
                      Name = name
                      Required = required
                      Limit = limit
                      Deductibles = deductibles
                      DefaultDeductible = defaultDeductible
                      Requires = requires
                      MinimumPremium = minimumPremium
                      Algorithm = algorithm }
            })

    let private action: Decoder<UnderwritingAction> =
        Decode.string
        |> Decode.andThen (fun s ->
            match invariant s with
            | "refer" -> Ok UnderwritingAction.Refer
            | "decline" -> Ok UnderwritingAction.Decline
            | other -> Error $"unknown action '{other}' (expected refer or decline)")

    let rule: Decoder<UnderwritingRule> =
        Decode.strictObject
            [ "id"; "description"; "when"; "action"; "message" ]
            (decoder {
                let! id = Decode.field "id" Decode.string
                and! description = Decode.withDefault "" "description" Decode.string
                and! condition = Decode.field "when" (syntax Parser.parseExpression source)
                and! action = Decode.field "action" action
                and! message = Decode.field "message" Decode.string

                return
                    { Id = id
                      Description = description
                      Condition = condition
                      Action = action
                      Message = message }
            })

    let fee: Decoder<Fee> =
        Decode.strictObject
            [ "code"; "name"; "amount" ]
            (decoder {
                let! code = Decode.field "code" Decode.string
                and! name = Decode.field "name" Decode.string
                and! amount = Decode.field "amount" Decode.decimal

                return
                    { Code = code
                      Name = name
                      Amount = amount }
            })

    let tax: Decoder<Tax> =
        Decode.strictObject
            [ "code"; "name"; "rate"; "includesFees" ]
            (decoder {
                let! code = Decode.field "code" Decode.string
                and! name = Decode.field "name" Decode.string
                and! rate = Decode.field "rate" Decode.decimal
                and! includesFees = Decode.withDefault false "includesFees" Decode.bool

                return
                    { Code = code
                      Name = name
                      Rate = rate
                      IncludesFees = includesFees }
            })

    let defaultPremiumRules =
        { Rounding = HalfUp
          Places = 0
          PolicyMinimum = 0m
          AnnualBasisDays = 365
          MinimumTermDays = 1
          MaximumTermDays = 366 }

    let premiumRules: Decoder<PremiumRules> =
        let d = defaultPremiumRules

        Decode.strictObject
            [ "rounding"
              "places"
              "policyMinimum"
              "annualBasisDays"
              "minimumTermDays"
              "maximumTermDays" ]
            (decoder {
                let! rounding =
                    Decode.withDefault d.Rounding "rounding" (Decode.string |> Decode.andThen RoundingMode.parse)

                and! places = Decode.withDefault d.Places "places" Decode.int
                and! policyMinimum = Decode.withDefault d.PolicyMinimum "policyMinimum" Decode.decimal
                and! basis = Decode.withDefault d.AnnualBasisDays "annualBasisDays" Decode.int
                and! minTerm = Decode.withDefault d.MinimumTermDays "minimumTermDays" Decode.int
                and! maxTerm = Decode.withDefault d.MaximumTermDays "maximumTermDays" Decode.int

                return
                    { Rounding = rounding
                      Places = places
                      PolicyMinimum = policyMinimum
                      AnnualBasisDays = basis
                      MinimumTermDays = minTerm
                      MaximumTermDays = maxTerm }
            })

    // ---- tables ---------------------------------------------------------------------------

    type private DimensionSpec = { Name: string; Kind: DimensionKind }

    let private extrapolation: Decoder<Extrapolation> =
        Decode.string
        |> Decode.andThen (fun s ->
            match invariant s with
            | "clamp" -> Ok Clamp
            | "linear" -> Ok Linear
            | "reject" -> Ok Reject
            | other -> Error $"unknown extrapolation '{other}' (expected clamp, linear or reject)")

    let private dimensionSpec: Decoder<DimensionSpec> =
        Decode.strictObject
            [ "name"; "kind"; "type"; "extrapolation" ]
            (decoder {
                let! name = Decode.field "name" Decode.string
                and! kind = Decode.withDefault "exact" "kind" Decode.string
                and! t = Decode.withDefault TText "type" dataType
                and! extra = Decode.withDefault Reject "extrapolation" extrapolation
                return name, invariant kind, t, extra
            }
            |> Decode.andThen (fun (name, kind, t, extra) ->
                match kind with
                | "exact" -> Ok { Name = name; Kind = Exact t }
                | "band"
                | "banded" -> Ok { Name = name; Kind = Banded }
                | "interpolate"
                | "interpolated" -> Ok { Name = name; Kind = Interpolated extra }
                | other -> Error $"unknown dimension kind '{other}' (expected exact, band or interpolate)"))

    /// Parses `"lo..hi"`, `"lo.."` or `"..hi"` into a band, lower bound inclusive.
    let parseBand (s: string) : Result<Band, string> =
        let parts = s.Split("..")

        let bound (text: string) =
            if String.isBlank text then
                Ok None
            else
                match Decimal.TryParse(text.Trim(), NumberStyles.Number, CultureInfo.InvariantCulture) with
                | true, d -> Ok(Some d)
                | _ -> Error $"'{text}' is not a number"

        if parts.Length <> 2 then
            Error $"'{s}' is not a band; write 'lo..hi', 'lo..' or '..hi'"
        else
            match bound parts[0], bound parts[1] with
            | Ok lo, Ok hi -> Ok { Lower = lo; Upper = hi }
            | Error e, _
            | _, Error e -> Error e

    let private keyFor (spec: DimensionSpec) : Decoder<DimensionKey> =
        match spec.Kind with
        | Exact t ->
            fun path e ->
                if e.ValueKind = JsonValueKind.String && e.GetString() = "*" then
                    Ok Wildcard
                else
                    valueOf t path e |> Result.map Key
        | Banded -> Decode.string |> Decode.andThen parseBand |> Decode.map BandKey
        | Interpolated _ -> Decode.decimal |> Decode.map Point

    let private row (specs: DimensionSpec list) : Decoder<DimensionKey list * decimal> =
        fun path e ->
            if e.ValueKind <> JsonValueKind.Array then
                Error [ { Path = path; Message = "a table row must be an array of keys followed by a value" } ]
            else
                let items = e.EnumerateArray() |> Array.ofSeq

                if items.Length <> specs.Length + 1 then
                    Error
                        [ { Path = path
                            Message = $"expected {specs.Length} key(s) and a value but found {items.Length} item(s)" } ]
                else
                    let keys =
                        List.zip specs (List.ofArray items[.. specs.Length - 1])
                        |> List.mapi (fun i (spec, x) -> keyFor spec $"{path}[{i}]" x)
                        |> Result.traverseAll id

                    Result.zip keys (Decode.decimal $"{path}[{specs.Length}]" items[specs.Length])

    /// Orders the keys found in rows: bands by lower bound, points ascending, exact keys in order
    /// of first appearance with any wildcard last.
    let private dimensionsFromRows (specs: DimensionSpec list) (rows: (DimensionKey list * decimal) list) =
        specs
        |> List.mapi (fun i spec ->
            let keys = rows |> List.map (fun (ks, _) -> ks[i]) |> List.distinct

            let ordered =
                match spec.Kind with
                | Exact _ -> (keys |> List.filter ((<>) Wildcard)) @ (keys |> List.filter ((=) Wildcard))
                | Banded ->
                    keys
                    |> List.sortBy (function
                        | BandKey b -> b.Lower |> Option.defaultValue Decimal.MinValue
                        | _ -> Decimal.MinValue)
                | Interpolated _ ->
                    keys
                    |> List.sortBy (function
                        | Point p -> p
                        | _ -> Decimal.MinValue)

            { Dimension.Name = spec.Name
              Kind = spec.Kind
              Keys = Array.ofList ordered })

    type private VersionSpec =
        | Rows of effective: DateOnly * label: string option * rows: (DimensionKey list * decimal) list
        | Derived of effective: DateOnly * label: string option * factor: decimal * places: int

    let private versionSpec (specs: DimensionSpec list) : Decoder<VersionSpec> =
        let derive =
            Decode.strictObject
                [ "factor"; "places" ]
                (decoder {
                    let! factor = Decode.field "factor" Decode.decimal
                    and! places = Decode.withDefault 4 "places" Decode.int
                    return factor, places
                })

        Decode.strictObject
            [ "effective"; "label"; "rows"; "derive" ]
            (decoder {
                let! effective = Decode.field "effective" Decode.date
                and! label = Decode.optional "label" Decode.string
                and! rows = Decode.optional "rows" (Decode.list (row specs))
                and! derived = Decode.optional "derive" derive
                return effective, label, rows, derived
            }
            |> Decode.andThen (fun (effective, label, rows, derived) ->
                match rows, derived with
                | Some r, None -> Ok(Rows(effective, label, r))
                | None, Some(factor, places) -> Ok(Derived(effective, label, factor, places))
                | Some _, Some _ -> Error "a version has either 'rows' or 'derive', not both"
                | None, None -> Error "a version needs 'rows' or 'derive'"))

    let private effectiveOf =
        function
        | Rows(d, _, _)
        | Derived(d, _, _, _) -> d

    let table: Decoder<VersionedTable> =
        Decode.strictObject
            [ "name"; "description"; "dimensions"; "versions" ]
            (decoder {
                let! name = Decode.field "name" Decode.string
                and! description = Decode.optional "description" Decode.string
                and! specs = Decode.field "dimensions" (Decode.list dimensionSpec)
                return name, description, specs
            }
            |> Decode.bind (fun (name, description, specs) ->
                Decode.field "versions" (Decode.list (versionSpec specs))
                |> Decode.andThen (fun versions ->
                    let ordered = versions |> List.sortBy effectiveOf

                    let built =
                        ordered
                        |> List.fold
                            (fun (acc: Result<TableVersion list, string list>) spec ->
                                acc
                                |> Result.bind (fun previous ->
                                    match spec with
                                    | Rows(effective, label, rows) ->
                                        let dims = dimensionsFromRows specs rows

                                        Table.build name dims (rows |> List.map (fun (k, v) -> { Keys = k; Value = v }))
                                        |> Result.mapError (List.map (fun e -> $"version {Dates.iso effective}: {e}"))
                                        |> Result.map (fun t ->
                                            previous
                                            @ [ { Effective = effective
                                                  Label = label
                                                  Table = t } ])
                                    | Derived(effective, label, factor, places) ->
                                        match List.tryLast previous with
                                        | None ->
                                            Error
                                                [ $"version {Dates.iso effective} derives from an earlier version, but there is none" ]
                                        | Some prior ->
                                            Ok(
                                                previous
                                                @ [ { Effective = effective
                                                      Label = label
                                                      Table = Table.scale factor places prior.Table } ]
                                            )))
                            (Ok [])

                    built
                    |> Result.bind (fun versions ->
                        VersionedTable.create name description versions
                        |> Result.mapError (List.map string))
                    |> Result.mapError (String.joinWith "; "))))

    let product: Decoder<Product> =
        Decode.strictObject
            [ "code"
              "name"
              "currency"
              "description"
              "available"
              "withdrawn"
              "quoteValidityDays"
              "attributes"
              "coverages"
              "tables"
              "rules"
              "fees"
              "taxes"
              "premium" ]
            (decoder {
                let! code = Decode.field "code" Decode.string
                and! name = Decode.field "name" Decode.string
                and! currency = Decode.withDefault "USD" "currency" Decode.string
                and! description = Decode.optional "description" Decode.string
                and! available = Decode.field "available" Decode.date
                and! withdrawn = Decode.optional "withdrawn" Decode.date
                and! validity = Decode.withDefault 30 "quoteValidityDays" Decode.int
                and! attributes = Decode.field "attributes" (Decode.list attribute)
                and! coverages = Decode.field "coverages" (Decode.list coverage)
                and! tables = Decode.withDefault [] "tables" (Decode.list table)
                and! rules = Decode.withDefault [] "rules" (Decode.list rule)
                and! fees = Decode.withDefault [] "fees" (Decode.list fee)
                and! taxes = Decode.withDefault [] "taxes" (Decode.list tax)
                and! premium = Decode.withDefault defaultPremiumRules "premium" premiumRules

                return
                    { Code = code
                      Name = name
                      Currency = currency
                      Description = description
                      Available = available
                      Withdrawn = withdrawn
                      QuoteValidityDays = validity
                      Attributes = attributes
                      Coverages = coverages
                      Tables = tables
                      Rules = rules
                      Fees = fees
                      Taxes = taxes
                      Premium = premium }
            })

    /// Decodes and validates a product, returning a rating plan or every problem found.
    let load (json: string) : Result<RatingPlan, string list> =
        match Decode.fromString product json with
        | Error errs -> Error(errs |> List.map string)
        | Ok p -> Product.plan p |> Result.mapError (List.map string)
