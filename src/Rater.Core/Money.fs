namespace Rater

open System

/// How a decimal amount is brought to a fixed number of places.
type RoundingMode =
    /// 0.5 goes away from zero (the convention most rate manuals state).
    | HalfUp
    /// 0.5 goes to the nearest even digit (banker's rounding).
    | HalfEven
    /// Always away from zero. Used for "round premium up to the next dollar" rules.
    | Up
    /// Always toward zero.
    | Down

[<RequireQualifiedAccess>]
module RoundingMode =

    let parse (s: string) =
        match invariant s with
        | "half_up"
        | "halfup" -> Ok HalfUp
        | "half_even"
        | "halfeven"
        | "bankers" -> Ok HalfEven
        | "up" -> Ok Up
        | "down" -> Ok Down
        | other -> Error $"unknown rounding mode '{other}' (expected half_up, half_even, up or down)"

    let name =
        function
        | HalfUp -> "half_up"
        | HalfEven -> "half_even"
        | Up -> "up"
        | Down -> "down"

[<RequireQualifiedAccess>]
module Decimal =

    let private pow10 (places: int) =
        let mutable p = 1m

        for _ in 1..places do
            p <- p * 10m

        p

    /// Rounds to a number of decimal places. Negative places round to tens, hundreds and so on,
    /// which rate manuals use for limits ("round to the nearest 1,000").
    let round (mode: RoundingMode) (places: int) (value: decimal) =
        if places >= 0 then
            let places = min places 28

            match mode with
            | HalfUp -> Math.Round(value, places, MidpointRounding.AwayFromZero)
            | HalfEven -> Math.Round(value, places, MidpointRounding.ToEven)
            | Up ->
                let p = pow10 places
                let scaled = value * p
                let t = Math.Truncate scaled

                (if t = scaled then t
                 elif value > 0m then t + 1m
                 else t - 1m)
                / p
            | Down -> Math.Truncate(value * pow10 places) / pow10 places
        else
            let p = pow10 -places

            match mode with
            | HalfUp -> Math.Round(value / p, 0, MidpointRounding.AwayFromZero) * p
            | HalfEven -> Math.Round(value / p, 0, MidpointRounding.ToEven) * p
            | Up ->
                let scaled = value / p
                let t = Math.Truncate scaled

                (if t = scaled then t
                 elif value > 0m then t + 1m
                 else t - 1m)
                * p
            | Down -> Math.Truncate(value / p) * p

    /// Normalises trailing zeros away so "1.50" and "1.5" print identically.
    let normalize (d: decimal) = d / 1.0000000000000000000000000000m

    let toInvariantString (d: decimal) =
        (normalize d).ToString(Globalization.CultureInfo.InvariantCulture)

/// A monetary amount in a single currency. Rating works in decimals throughout so that
/// premiums reproduce exactly against a rate manual worksheet.
[<Struct; CustomEquality; CustomComparison>]
type Money =
    { Amount: decimal
      Currency: string }

    override m.ToString() =
        $"""{m.Currency} {m.Amount.ToString("0.00", Globalization.CultureInfo.InvariantCulture)}"""

    override m.Equals(o) =
        match o with
        | :? Money as other -> m.Amount = other.Amount && m.Currency = other.Currency
        | _ -> false

    override m.GetHashCode() = HashCode.Combine(m.Amount, m.Currency)

    interface IComparable with
        member m.CompareTo(o) =
            match o with
            | :? Money as other ->
                if m.Currency <> other.Currency then
                    invalidArg "o" $"cannot compare {m.Currency} with {other.Currency}"

                compare m.Amount other.Amount
            | _ -> invalidArg "o" "not a Money value"

    static member private Check (a: Money) (b: Money) =
        if a.Currency <> b.Currency then
            invalidOp $"currency mismatch: {a.Currency} and {b.Currency}"

    static member (+)(a: Money, b: Money) =
        Money.Check a b

        { Amount = a.Amount + b.Amount
          Currency = a.Currency }

    static member (-)(a: Money, b: Money) =
        Money.Check a b

        { Amount = a.Amount - b.Amount
          Currency = a.Currency }

    static member (*)(a: Money, k: decimal) =
        { Amount = a.Amount * k
          Currency = a.Currency }

[<RequireQualifiedAccess>]
module Money =

    let create currency amount =
        { Amount = amount; Currency = currency }

    let zero currency = create currency 0m

    let amount (m: Money) = m.Amount

    let round mode places (m: Money) =
        { m with
            Amount = Decimal.round mode places m.Amount }

    let sum currency (items: Money seq) = Seq.fold (+) (zero currency) items

    let max (a: Money) (b: Money) = if a.Amount >= b.Amount then a else b

    /// Splits `total` into parts proportional to `weights`, each a whole number of the minor unit
    /// (10^-places), so that the parts always add back to `total` exactly. Remainders go to the
    /// parts with the largest fractional shares (Hamilton / largest remainder method); ties go to
    /// the earliest part, which keeps the output deterministic.
    let allocate (places: int) (total: decimal) (weights: decimal list) : decimal list =
        if List.isEmpty weights then
            invalidArg "weights" "at least one weight is required"

        if weights |> List.exists (fun w -> w < 0m) then
            invalidArg "weights" "weights must be non-negative"

        let unit = 1m / pown 10m places
        let totalUnits = Decimal.round HalfUp places total / unit
        let weightSum = List.sum weights
        let n = List.length weights

        let shares =
            if weightSum = 0m then
                List.replicate n (totalUnits / decimal n)
            else
                weights |> List.map (fun w -> totalUnits * w / weightSum)

        let floors =
            shares
            |> List.map (fun s -> if totalUnits >= 0m then Math.Floor s else Math.Ceiling s)

        let distributed = List.sum floors
        let leftover = int (abs (totalUnits - distributed))
        let step = if totalUnits >= 0m then 1m else -1m

        let bonus =
            List.zip shares floors
            |> List.mapi (fun i (s, f) -> i, abs (s - f))
            |> List.sortBy (fun (i, frac) -> -frac, i)
            |> List.take leftover
            |> List.map fst
            |> Set.ofList

        floors
        |> List.mapi (fun i f -> (if bonus.Contains i then f + step else f) * unit)
