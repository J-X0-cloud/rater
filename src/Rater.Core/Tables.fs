namespace Rater

open System

/// What happens when an interpolated dimension is asked for a point outside its breakpoints.
type Extrapolation =
    /// Use the nearest end value.
    | Clamp
    /// Extend the slope of the nearest segment.
    | Linear
    /// Refuse to rate; the risk is outside the filed curve.
    | Reject

/// A numeric band. The lower bound is inclusive and the upper bound exclusive, so adjacent
/// bands such as [0, 1000) and [1000, 5000) never overlap. Missing bounds are open-ended.
type Band =
    { Lower: decimal option
      Upper: decimal option }

    member b.Contains(x: decimal) =
        (match b.Lower with
         | Some lo -> x >= lo
         | None -> true)
        && (match b.Upper with
            | Some hi -> x < hi
            | None -> true)

    override b.ToString() =
        let lo =
            b.Lower |> Option.map Decimal.toInvariantString |> Option.defaultValue "-inf"

        let hi =
            b.Upper |> Option.map Decimal.toInvariantString |> Option.defaultValue "+inf"

        $"[{lo}, {hi})"

/// How the keys of one table dimension are matched against an argument.
type DimensionKind =
    /// Keys are discrete values of one type; an optional wildcard catches everything else.
    | Exact of DataType
    /// Keys are numeric bands.
    | Banded
    /// Keys are numeric breakpoints; values between them are linearly interpolated.
    | Interpolated of Extrapolation

type DimensionKey =
    | Key of Value
    | Wildcard
    | BandKey of Band
    | Point of decimal

    override k.ToString() =
        match k with
        | Key v -> string v
        | Wildcard -> "*"
        | BandKey b -> string b
        | Point p -> Decimal.toInvariantString p

type Dimension =
    { Name: string
      Kind: DimensionKind
      Keys: DimensionKey array }

    /// The type of argument this dimension accepts.
    member d.ArgumentType =
        match d.Kind with
        | Exact t -> t
        | Banded
        | Interpolated _ -> TNumber

/// A dense N-dimensional rating table stored row-major. Every combination of keys has a cell;
/// that invariant is established by `Table.build` and relied upon by `Table.lookup`.
type Table =
    private
        { name: string
          dimensions: Dimension array
          cells: decimal array
          strides: int array }

    member t.Name = t.name
    member t.Dimensions = List.ofArray t.dimensions
    member t.CellCount = t.cells.Length

type TableRow =
    { Keys: DimensionKey list
      Value: decimal }

type TableError =
    | NoDimensions of table: string
    | DuplicateDimension of table: string * dimension: string
    | EmptyDimension of table: string * dimension: string
    | KeyTypeMismatch of table: string * dimension: string * key: string * expected: DataType
    | DuplicateKey of table: string * dimension: string * key: string
    | OverlappingBands of table: string * dimension: string * Band * Band
    | UnsortedBreakpoints of table: string * dimension: string
    | RowArity of table: string * row: int * expected: int * actual: int
    | UnknownRowKey of table: string * row: int * dimension: string * key: string
    | DuplicateCell of table: string * keys: string list
    | MissingCells of table: string * missing: string list * total: int

    override e.ToString() =
        match e with
        | NoDimensions t -> $"table '{t}' has no dimensions"
        | DuplicateDimension(t, d) -> $"table '{t}' declares dimension '{d}' more than once"
        | EmptyDimension(t, d) -> $"table '{t}' dimension '{d}' has no keys"
        | KeyTypeMismatch(t, d, k, ty) -> $"table '{t}' dimension '{d}': key {k} is not a {ty}"
        | DuplicateKey(t, d, k) -> $"table '{t}' dimension '{d}': key {k} appears more than once"
        | OverlappingBands(t, d, a, b) -> $"table '{t}' dimension '{d}': bands {a} and {b} overlap"
        | UnsortedBreakpoints(t, d) ->
            $"table '{t}' dimension '{d}': interpolation breakpoints must be strictly increasing"
        | RowArity(t, r, e, a) -> $"table '{t}' row {r}: expected {e} keys but found {a}"
        | UnknownRowKey(t, r, d, k) -> $"table '{t}' row {r}: {k} is not a key of dimension '{d}'"
        | DuplicateCell(t, ks) -> $"""table '{t}': cell ({String.joinWith ", " ks}) is defined twice"""
        | MissingCells(t, ms, total) ->
            let shown = ms |> List.truncate 5 |> String.joinWith "; "
            let more = if total > 5 then $" and {total - 5} more" else ""
            $"table '{t}' is missing {total} cell(s): {shown}{more}"

/// The outcome of resolving one argument against one dimension.
type DimensionMatch =
    | Hit of index: int * key: DimensionKey
    | Between of lowIndex: int * highIndex: int * weight: decimal
    | Extrapolated of lowIndex: int * highIndex: int * weight: decimal

type LookupError =
    | ArgumentCount of table: string * expected: int * actual: int
    | ArgumentType of table: string * dimension: string * expected: DataType * actual: Value
    | NoMatchingKey of table: string * dimension: string * argument: Value
    | OutOfRange of table: string * dimension: string * argument: decimal * low: decimal * high: decimal

    override e.ToString() =
        match e with
        | ArgumentCount(t, ex, ac) -> $"table '{t}' takes {ex} argument(s) but was given {ac}"
        | ArgumentType(t, d, ex, v) ->
            $"table '{t}' dimension '{d}' expects a {ex} but was given {Value.describe v}"
        | NoMatchingKey(t, d, v) -> $"table '{t}' has no row for {d} = {Value.describe v}"
        | OutOfRange(t, d, x, lo, hi) ->
            $"table '{t}' dimension '{d}': {Decimal.toInvariantString x} is outside the rated range \
              [{Decimal.toInvariantString lo}, {Decimal.toInvariantString hi}]"

/// Explains how a lookup arrived at its value; rendered into the rating worksheet.
type LookupTrace =
    { Table: string
      Arguments: (string * Value) list
      Matches: DimensionMatch list
      /// One human-readable line per dimension, e.g. "limit: between 250000 and 300000 (weight 0.5)".
      Explanation: string list
      Result: decimal }

[<RequireQualifiedAccess>]
module Table =

    let private keyLabel (d: Dimension) (k: DimensionKey) = $"{d.Name}={k}"

    let private validateDimension (table: string) (d: Dimension) : TableError list =
        let keys = List.ofArray d.Keys

        if keys.IsEmpty then
            [ EmptyDimension(table, d.Name) ]
        else
            match d.Kind with
            | Exact t ->
                let typeErrors =
                    keys
                    |> List.choose (fun k ->
                        match k with
                        | Key v when v.Type = t -> None
                        | Wildcard -> None
                        | other -> Some(KeyTypeMismatch(table, d.Name, string other, t)))

                let dups =
                    keys
                    |> List.countBy id
                    |> List.filter (fun (_, n) -> n > 1)
                    |> List.map (fun (k, _) -> DuplicateKey(table, d.Name, string k))

                typeErrors @ dups
            | Banded ->
                let bands =
                    keys
                    |> List.choose (function
                        | BandKey b -> Some b
                        | _ -> None)

                if bands.Length <> keys.Length then
                    [ KeyTypeMismatch(table, d.Name, "non-band key", TNumber) ]
                else
                    let lowerOf b =
                        b.Lower |> Option.defaultValue Decimal.MinValue

                    let sorted = bands |> List.sortBy lowerOf

                    let inverted =
                        bands
                        |> List.filter (fun b ->
                            match b.Lower, b.Upper with
                            | Some lo, Some hi -> lo >= hi
                            | _ -> false)
                        |> List.map (fun b -> OverlappingBands(table, d.Name, b, b))

                    let overlaps =
                        sorted
                        |> List.pairwise
                        |> List.choose (fun (a, b) ->
                            let aUpper = a.Upper |> Option.defaultValue Decimal.MaxValue

                            match b.Lower with
                            | Some lo when lo >= aUpper -> None
                            | _ -> Some(OverlappingBands(table, d.Name, a, b)))

                    inverted @ overlaps
            | Interpolated _ ->
                let points =
                    keys
                    |> List.choose (function
                        | Point p -> Some p
                        | _ -> None)

                if points.Length <> keys.Length then
                    [ KeyTypeMismatch(table, d.Name, "non-numeric breakpoint", TNumber) ]
                elif points |> List.pairwise |> List.exists (fun (a, b) -> a >= b) then
                    [ UnsortedBreakpoints(table, d.Name) ]
                else
                    []

    /// Index of a row key within a dimension, honouring numeric equality for points and keys.
    let private indexOfKey (d: Dimension) (k: DimensionKey) =
        d.Keys |> Array.tryFindIndex (fun existing -> existing = k)

    let private strideOf (dims: Dimension array) =
        let strides = Array.zeroCreate dims.Length
        let mutable acc = 1

        for i in dims.Length - 1 .. -1 .. 0 do
            strides[i] <- acc
            acc <- acc * dims[i].Keys.Length

        strides

    let private describeIndex (dims: Dimension array) (strides: int array) (flat: int) =
        dims
        |> Array.mapi (fun i d -> keyLabel d d.Keys[(flat / strides[i]) % d.Keys.Length])
        |> List.ofArray

    /// Builds a dense table from sparse rows, reporting every structural problem at once.
    let build (name: string) (dimensions: Dimension list) (rows: TableRow list) : Result<Table, TableError list> =
        let dims = Array.ofList dimensions

        if dims.Length = 0 then
            Error [ NoDimensions name ]
        else
            let dimErrors =
                let dupNames =
                    dimensions
                    |> List.countBy (fun d -> d.Name)
                    |> List.filter (fun (_, n) -> n > 1)
                    |> List.map (fun (n, _) -> DuplicateDimension(name, n))

                dupNames @ (dimensions |> List.collect (validateDimension name))

            if not dimErrors.IsEmpty then
                Error dimErrors
            else
                let strides = strideOf dims
                let size = dims |> Array.fold (fun acc d -> acc * d.Keys.Length) 1
                let cells = Array.zeroCreate<decimal> size
                let filled = Array.zeroCreate<bool> size
                let errors = ResizeArray<TableError>()

                rows
                |> List.iteri (fun rowNo row ->
                    let rowNo = rowNo + 1

                    if row.Keys.Length <> dims.Length then
                        errors.Add(RowArity(name, rowNo, dims.Length, row.Keys.Length))
                    else
                        let indexes =
                            List.zip (List.ofArray dims) row.Keys
                            |> List.map (fun (d, k) ->
                                match indexOfKey d k with
                                | Some i -> Ok i
                                | None -> Error(UnknownRowKey(name, rowNo, d.Name, string k)))

                        match indexes |> List.choose (function Error e -> Some e | Ok _ -> None) with
                        | [] ->
                            let flat =
                                indexes
                                |> List.mapi (fun i r ->
                                    match r with
                                    | Ok idx -> idx * strides[i]
                                    | Error _ -> 0)
                                |> List.sum

                            if filled[flat] then
                                errors.Add(DuplicateCell(name, describeIndex dims strides flat))
                            else
                                filled[flat] <- true
                                cells[flat] <- row.Value
                        | es -> errors.AddRange es)

                let missing =
                    filled
                    |> Array.mapi (fun i f -> i, f)
                    |> Array.filter (fun (_, f) -> not f)
                    |> Array.map fst

                if missing.Length > 0 then
                    let described =
                        missing
                        |> Array.truncate 5
                        |> Array.map (fun i -> describeIndex dims strides i |> String.joinWith ", ")
                        |> List.ofArray

                    errors.Add(MissingCells(name, described, missing.Length))

                if errors.Count > 0 then
                    Error(List.ofSeq errors)
                else
                    Ok
                        { name = name
                          dimensions = dims
                          cells = cells
                          strides = strides }

    /// Every cell as (keys, value), in row-major order.
    let rows (t: Table) : TableRow list =
        [ for flat in 0 .. t.cells.Length - 1 ->
              { Keys =
                  t.dimensions
                  |> Array.mapi (fun i d -> d.Keys[(flat / t.strides[i]) % d.Keys.Length])
                  |> List.ofArray
                Value = t.cells[flat] } ]

    /// Binary search for the band containing x. Bands are validated as non-overlapping.
    let private findBand (keys: DimensionKey array) (x: decimal) =
        let sorted =
            keys
            |> Array.mapi (fun i k ->
                match k with
                | BandKey b -> i, b
                | _ -> invalidOp "banded dimension holds a non-band key")
            |> Array.sortBy (fun (_, b) -> b.Lower |> Option.defaultValue Decimal.MinValue)

        let rec search lo hi =
            if lo > hi then
                None
            else
                let mid = lo + (hi - lo) / 2
                let idx, band = sorted[mid]

                if band.Contains x then
                    Some(idx, band)
                else
                    match band.Lower with
                    | Some l when x < l -> search lo (mid - 1)
                    | _ -> search (mid + 1) hi

        search 0 (sorted.Length - 1)

    let private matchDimension (table: string) (d: Dimension) (arg: Value) : Result<DimensionMatch, LookupError> =
        match d.Kind, arg with
        | Exact t, v when v.Type = t ->
            match d.Keys |> Array.tryFindIndex (fun k -> k = Key v) with
            | Some i -> Ok(Hit(i, d.Keys[i]))
            | None ->
                match d.Keys |> Array.tryFindIndex (fun k -> k = Wildcard) with
                | Some i -> Ok(Hit(i, Wildcard))
                | None -> Error(NoMatchingKey(table, d.Name, v))
        | Exact t, v -> Error(ArgumentType(table, d.Name, t, v))
        | Banded, VNumber x ->
            match findBand d.Keys x with
            | Some(i, b) -> Ok(Hit(i, BandKey b))
            | None -> Error(NoMatchingKey(table, d.Name, arg))
        | Interpolated mode, VNumber x ->
            let points =
                d.Keys
                |> Array.map (function
                    | Point p -> p
                    | _ -> invalidOp "interpolated dimension holds a non-point key")

            let last = points.Length - 1

            if points.Length = 1 then
                if x = points[0] || mode = Clamp then
                    Ok(Hit(0, d.Keys[0]))
                else
                    Error(OutOfRange(table, d.Name, x, points[0], points[0]))
            else
                match Array.BinarySearch(points, x) with
                | i when i >= 0 -> Ok(Hit(i, d.Keys[i]))
                | complement ->
                    let insertAt = ~~~complement

                    if insertAt = 0 || insertAt > last then
                        let lo, hi = if insertAt = 0 then 0, 1 else last - 1, last

                        match mode with
                        | Reject -> Error(OutOfRange(table, d.Name, x, points[0], points[last]))
                        | Clamp ->
                            let i = if insertAt = 0 then 0 else last
                            Ok(Hit(i, d.Keys[i]))
                        | Linear ->
                            let w = (x - points[lo]) / (points[hi] - points[lo])
                            Ok(Extrapolated(lo, hi, w))
                    else
                        let lo, hi = insertAt - 1, insertAt
                        let w = (x - points[lo]) / (points[hi] - points[lo])
                        Ok(Between(lo, hi, w))
        | (Banded | Interpolated _), v -> Error(ArgumentType(table, d.Name, TNumber, v))

    let describeMatch (t: Table) (dimIndex: int) (m: DimensionMatch) =
        let d = t.dimensions[dimIndex]

        match m with
        | Hit(_, k) -> $"{d.Name}: {k}"
        | Between(lo, hi, w)
        | Extrapolated(lo, hi, w) ->
            let verb =
                match m with
                | Extrapolated _ -> "extrapolated"
                | _ -> "between"

            $"{d.Name}: {verb} {d.Keys[lo]} and {d.Keys[hi]} (weight {Decimal.toInvariantString (Decimal.round HalfUp 6 w)})"

    /// Multilinear lookup: exact and banded dimensions pin a single index, interpolated
    /// dimensions contribute two corners each, and the result is the weighted sum over all corners.
    let lookup (t: Table) (args: Value list) : Result<LookupTrace, LookupError> =
        if args.Length <> t.dimensions.Length then
            Error(ArgumentCount(t.name, t.dimensions.Length, args.Length))
        else
            let matches =
                List.zip (List.ofArray t.dimensions) args
                |> Result.traverse (fun (d, a) -> matchDimension t.name d a)

            matches
            |> Result.map (fun ms ->
                let corners =
                    ms
                    |> List.mapi (fun dim m ->
                        let s = t.strides[dim]

                        match m with
                        | Hit(i, _) -> [ i * s, 1m ]
                        | Between(lo, hi, w)
                        | Extrapolated(lo, hi, w) -> [ lo * s, 1m - w; hi * s, w ])

                let rec expand (acc: (int * decimal) list) (rest: (int * decimal) list list) =
                    match rest with
                    | [] -> acc
                    | opts :: more ->
                        let next =
                            [ for offset, weight in acc do
                                  for o, w in opts do
                                      if w <> 0m || opts.Length = 1 then
                                          yield offset + o, weight * w ]

                        expand next more

                let value =
                    expand [ 0, 1m ] corners |> List.sumBy (fun (flat, w) -> t.cells[flat] * w)

                { Table = t.name
                  Arguments = List.zip (t.dimensions |> Array.map (fun d -> d.Name) |> List.ofArray) args
                  Matches = ms
                  Explanation = ms |> List.mapi (describeMatch t)
                  Result = value })

    /// Two tables can be swapped for one another without re-checking algorithms when their
    /// dimension names, kinds and argument types agree.
    let signature (t: Table) =
        t.dimensions |> Array.map (fun d -> d.Name, d.Kind) |> List.ofArray

    /// Applies a factor to every cell; used for across-the-board rate changes.
    let scale (factor: decimal) (places: int) (t: Table) =
        { t with
            cells = t.cells |> Array.map (fun c -> Decimal.round HalfUp places (c * factor)) }
