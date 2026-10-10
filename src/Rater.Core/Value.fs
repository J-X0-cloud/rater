namespace Rater

open System
open System.Globalization

/// The static types of the rating language. Every risk attribute, table key and expression has one.
type DataType =
    | TNumber
    | TText
    | TBool
    | TDate

    override t.ToString() =
        match t with
        | TNumber -> "number"
        | TText -> "text"
        | TBool -> "bool"
        | TDate -> "date"

/// A runtime value. Numbers are decimals so that factor chains multiply exactly.
[<CustomEquality; CustomComparison>]
type Value =
    | VNumber of decimal
    | VText of string
    | VBool of bool
    | VDate of DateOnly

    member v.Type =
        match v with
        | VNumber _ -> TNumber
        | VText _ -> TText
        | VBool _ -> TBool
        | VDate _ -> TDate

    /// Numbers compare by value regardless of scale, so 1.0 and 1 are the same table key.
    override v.Equals(o) =
        match o with
        | :? Value as other ->
            match v, other with
            | VNumber a, VNumber b -> a = b
            | VText a, VText b -> String.Equals(a, b, StringComparison.Ordinal)
            | VBool a, VBool b -> a = b
            | VDate a, VDate b -> a = b
            | _ -> false
        | _ -> false

    override v.GetHashCode() =
        match v with
        | VNumber n -> (Decimal.normalize n).GetHashCode()
        | VText s -> s.GetHashCode()
        | VBool b -> b.GetHashCode()
        | VDate d -> d.GetHashCode()

    interface IComparable with
        member v.CompareTo(o) =
            let rank =
                function
                | VNumber _ -> 0
                | VText _ -> 1
                | VBool _ -> 2
                | VDate _ -> 3

            match o with
            | :? Value as other ->
                match v, other with
                | VNumber a, VNumber b -> compare a b
                | VText a, VText b -> String.CompareOrdinal(a, b)
                | VBool a, VBool b -> compare a b
                | VDate a, VDate b -> compare a b
                | a, b -> compare (rank a) (rank b)
            | _ -> invalidArg "o" "not a Value"

    override v.ToString() =
        match v with
        | VNumber n -> Decimal.toInvariantString n
        | VText s -> s
        | VBool b -> if b then "true" else "false"
        | VDate d -> d.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture)

[<RequireQualifiedAccess>]
module DataType =

    let parse (s: string) =
        match invariant s with
        | "number"
        | "decimal"
        | "integer" -> Ok TNumber
        | "text"
        | "string" -> Ok TText
        | "bool"
        | "boolean" -> Ok TBool
        | "date" -> Ok TDate
        | other -> Error $"unknown type '{other}' (expected number, text, bool or date)"

[<RequireQualifiedAccess>]
module Value =

    let parseDate (s: string) =
        match DateOnly.TryParseExact(s, "yyyy-MM-dd", CultureInfo.InvariantCulture, DateTimeStyles.None) with
        | true, d -> Some d
        | _ -> None

    /// Parses a textual representation into a value of the requested type.
    let parseAs (t: DataType) (s: string) =
        match t with
        | TText -> Ok(VText s)
        | TNumber ->
            match Decimal.TryParse(s, NumberStyles.Number, CultureInfo.InvariantCulture) with
            | true, d -> Ok(VNumber d)
            | _ -> Error $"'{s}' is not a number"
        | TBool ->
            match invariant s with
            | "true"
            | "yes"
            | "y" -> Ok(VBool true)
            | "false"
            | "no"
            | "n" -> Ok(VBool false)
            | _ -> Error $"'{s}' is not a boolean"
        | TDate ->
            match parseDate s with
            | Some d -> Ok(VDate d)
            | None -> Error $"'{s}' is not a date in yyyy-MM-dd form"

    let describe (v: Value) =
        match v with
        | VText s -> $"\"{s}\""
        | other -> string other

    let asNumber =
        function
        | VNumber n -> Some n
        | _ -> None

    let asText =
        function
        | VText s -> Some s
        | _ -> None

    let asBool =
        function
        | VBool b -> Some b
        | _ -> None

    let asDate =
        function
        | VDate d -> Some d
        | _ -> None

[<RequireQualifiedAccess>]
module Dates =

    /// ISO 8601 calendar date, the only date format the engine reads or writes.
    let iso (d: DateOnly) =
        d.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture)

    /// Whole years between two dates, counting a year only once its anniversary has passed.
    /// This is how "age of home" and "driver age" are measured in rate manuals.
    let rec yearsBetween (fromDate: DateOnly) (toDate: DateOnly) =
        if toDate < fromDate then
            -(yearsBetween toDate fromDate)
        else
            let years = toDate.Year - fromDate.Year

            if fromDate.AddYears(years) > toDate then
                years - 1
            else
                years

    let daysBetween (fromDate: DateOnly) (toDate: DateOnly) = toDate.DayNumber - fromDate.DayNumber

    /// Whole months between two dates, using the same anniversary rule as `yearsBetween`.
    let monthsBetween (fromDate: DateOnly) (toDate: DateOnly) =
        let sign, a, b =
            if toDate < fromDate then
                -1, toDate, fromDate
            else
                1, fromDate, toDate

        let mutable months = (b.Year - a.Year) * 12 + b.Month - a.Month

        if a.AddMonths(months) > b then
            months <- months - 1

        sign * months
