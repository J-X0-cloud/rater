namespace Rater.Dsl

open System
open Rater

/// A runtime failure inside a built-in; carries a message suitable for the worksheet.
exception BuiltinError of string

/// The type of a built-in: one or more fixed overloads, or a variadic numeric function.
type Signature =
    | Overloads of (DataType list * DataType) list
    | VariadicNumbers of minimumArity: int

type Builtin =
    { Name: string
      Signature: Signature
      Doc: string
      Invoke: Value list -> Value }

[<RequireQualifiedAccess>]
module Builtins =

    let private num name =
        function
        | VNumber n -> n
        | v -> raise (BuiltinError $"{name} expects a number but got {Value.describe v}")

    let private integer name (v: Value) =
        let n = num name v

        if n <> Math.Truncate n then
            raise (BuiltinError $"{name} expects a whole number but got {Decimal.toInvariantString n}")

        if n > decimal Int32.MaxValue || n < decimal Int32.MinValue then
            raise (BuiltinError $"{name}: {Decimal.toInvariantString n} is out of range")

        int n

    let private date name =
        function
        | VDate d -> d
        | v -> raise (BuiltinError $"{name} expects a date but got {Value.describe v}")

    let private text name =
        function
        | VText s -> s
        | v -> raise (BuiltinError $"{name} expects text but got {Value.describe v}")

    let private places name (v: Value) =
        let p = integer name v

        if p < -12 || p > 12 then
            raise (BuiltinError $"{name}: places must be between -12 and 12")

        p

    let private rounding name mode =
        { Name = name
          Signature = Overloads [ [ TNumber ], TNumber; [ TNumber; TNumber ], TNumber ]
          Doc = $"{name}(x[, places]) rounds x ({RoundingMode.name mode}) to the given number of places (default 0)"
          Invoke =
            function
            | [ x ] -> VNumber(Decimal.round mode 0 (num name x))
            | [ x; p ] -> VNumber(Decimal.round mode (places name p) (num name x))
            | _ -> raise (BuiltinError $"{name} takes one or two arguments") }

    /// Integer powers by repeated squaring so the result stays an exact decimal.
    let power (b: decimal) (e: int) =
        let rec go (acc: decimal) (b: decimal) (e: int) =
            if e = 0 then acc
            elif e % 2 = 1 then go (acc * b) (b * b) (e / 2)
            else go acc (b * b) (e / 2)

        if e >= 0 then
            go 1m b e
        elif b = 0m then
            raise (BuiltinError "pow: zero cannot be raised to a negative power")
        else
            1m / go 1m b (-e)

    let private overflowGuard name f =
        try
            f ()
        with :? OverflowException ->
            raise (BuiltinError $"{name}: arithmetic overflow")

    let all: Builtin list =
        [ { Name = "min"
            Signature = VariadicNumbers 2
            Doc = "min(a, b, ...) is the smallest argument"
            Invoke = fun args -> VNumber(args |> List.map (num "min") |> List.min) }
          { Name = "max"
            Signature = VariadicNumbers 2
            Doc = "max(a, b, ...) is the largest argument"
            Invoke = fun args -> VNumber(args |> List.map (num "max") |> List.max) }
          { Name = "clamp"
            Signature = Overloads [ [ TNumber; TNumber; TNumber ], TNumber ]
            Doc = "clamp(x, lo, hi) limits x to the closed range [lo, hi]"
            Invoke =
              fun args ->
                  match args |> List.map (num "clamp") with
                  | [ x; lo; hi ] ->
                      if lo > hi then
                          raise (BuiltinError "clamp: lower bound exceeds upper bound")

                      VNumber(max lo (min hi x))
                  | _ -> raise (BuiltinError "clamp takes three arguments") }
          { Name = "abs"
            Signature = Overloads [ [ TNumber ], TNumber ]
            Doc = "abs(x) is the absolute value of x"
            Invoke = fun args -> VNumber(abs (num "abs" args.Head)) }
          { Name = "floor"
            Signature = Overloads [ [ TNumber ], TNumber ]
            Doc = "floor(x) is the largest whole number not greater than x"
            Invoke = fun args -> VNumber(Math.Floor(num "floor" args.Head)) }
          { Name = "ceil"
            Signature = Overloads [ [ TNumber ], TNumber ]
            Doc = "ceil(x) is the smallest whole number not less than x"
            Invoke = fun args -> VNumber(Math.Ceiling(num "ceil" args.Head)) }
          { Name = "pow"
            Signature = Overloads [ [ TNumber; TNumber ], TNumber ]
            Doc = "pow(x, n) raises x to a whole-number power n; used for compounding trend factors"
            Invoke =
              fun args ->
                  match args with
                  | [ b; e ] ->
                      let e = integer "pow" e

                      if abs e > 100 then
                          raise (BuiltinError "pow: exponent must be between -100 and 100")

                      overflowGuard "pow" (fun () -> VNumber(power (num "pow" b) e))
                  | _ -> raise (BuiltinError "pow takes two arguments") }
          rounding "round" HalfUp
          rounding "round_even" HalfEven
          rounding "round_up" Up
          rounding "round_down" Down
          { Name = "years_between"
            Signature = Overloads [ [ TDate; TDate ], TNumber ]
            Doc = "years_between(from, to) counts whole years, a year completing on its anniversary"
            Invoke =
              fun args ->
                  VNumber(decimal (Dates.yearsBetween (date "years_between" args[0]) (date "years_between" args[1]))) }
          { Name = "months_between"
            Signature = Overloads [ [ TDate; TDate ], TNumber ]
            Doc = "months_between(from, to) counts whole months"
            Invoke =
              fun args ->
                  VNumber(
                      decimal (Dates.monthsBetween (date "months_between" args[0]) (date "months_between" args[1]))
                  ) }
          { Name = "days_between"
            Signature = Overloads [ [ TDate; TDate ], TNumber ]
            Doc = "days_between(from, to) is the signed number of days from one date to the other"
            Invoke =
              fun args ->
                  VNumber(decimal (Dates.daysBetween (date "days_between" args[0]) (date "days_between" args[1]))) }
          { Name = "year"
            Signature = Overloads [ [ TDate ], TNumber ]
            Doc = "year(d) is the calendar year of d"
            Invoke = fun args -> VNumber(decimal (date "year" args.Head).Year) }
          { Name = "month"
            Signature = Overloads [ [ TDate ], TNumber ]
            Doc = "month(d) is the month number (1-12) of d"
            Invoke = fun args -> VNumber(decimal (date "month" args.Head).Month) }
          { Name = "add_years"
            Signature = Overloads [ [ TDate; TNumber ], TDate ]
            Doc = "add_years(d, n) shifts d by n whole years"
            Invoke =
              fun args ->
                  let d = date "add_years" args[0]
                  let n = integer "add_years" args[1]

                  try
                      VDate(d.AddYears n)
                  with :? ArgumentOutOfRangeException ->
                      raise (BuiltinError "add_years: result is outside the supported date range") }
          { Name = "lower"
            Signature = Overloads [ [ TText ], TText ]
            Doc = "lower(s) converts text to lower case"
            Invoke = fun args -> VText((text "lower" args.Head).ToLowerInvariant()) }
          { Name = "upper"
            Signature = Overloads [ [ TText ], TText ]
            Doc = "upper(s) converts text to upper case"
            Invoke = fun args -> VText((text "upper" args.Head).ToUpperInvariant()) }
          { Name = "starts_with"
            Signature = Overloads [ [ TText; TText ], TBool ]
            Doc = "starts_with(s, prefix) tests whether s begins with prefix"
            Invoke =
              fun args ->
                  VBool((text "starts_with" args[0]).StartsWith(text "starts_with" args[1], StringComparison.Ordinal)) }
          { Name = "length"
            Signature = Overloads [ [ TText ], TNumber ]
            Doc = "length(s) is the number of characters in s"
            Invoke = fun args -> VNumber(decimal (text "length" args.Head).Length) } ]

    let private byName = all |> List.map (fun b -> b.Name, b) |> Map.ofList

    let tryFind name = Map.tryFind name byName

    let names = all |> List.map (fun b -> b.Name)

    /// Resolves the result type of a call, or explains why the arguments do not fit.
    let resolve (b: Builtin) (argTypes: DataType list) : Result<DataType, string> =
        let shown (ts: DataType list) =
            ts |> List.map string |> String.joinWith ", "

        match b.Signature with
        | VariadicNumbers minimum ->
            if argTypes.Length < minimum then
                Error $"{b.Name} needs at least {minimum} arguments but was given {argTypes.Length}"
            else
                match argTypes |> List.tryFindIndex (fun t -> t <> TNumber) with
                | Some i -> Error $"{b.Name} argument {i + 1} must be a number but is a {argTypes[i]}"
                | None -> Ok TNumber
        | Overloads overloads ->
            match overloads |> List.tryFind (fun (ps, _) -> ps = argTypes) with
            | Some(_, ret) -> Ok ret
            | None ->
                let expected =
                    overloads |> List.map (fun (ps, _) -> $"{b.Name}({shown ps})") |> String.joinWith " or "

                Error $"{b.Name}({shown argTypes}) does not match {expected}"
