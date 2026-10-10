namespace Rater

open System

/// Small helpers shared across the engine. Kept deliberately tiny: anything that grows
/// domain meaning belongs in its own module.
[<AutoOpen>]
module Prelude =

    /// Applies a function to a value and returns the value; handy in pipelines.
    let inline tee f x =
        f x
        x

    let inline flip f a b = f b a

    let invariant (s: string) = s.ToLowerInvariant()

[<RequireQualifiedAccess>]
module Result =

    /// Collects every error instead of stopping at the first one. The success list keeps input order.
    let traverseAll (f: 'a -> Result<'b, 'e list>) (items: 'a seq) : Result<'b list, 'e list> =
        let oks = ResizeArray()
        let errs = ResizeArray()

        for item in items do
            match f item with
            | Ok v -> oks.Add v
            | Error es -> errs.AddRange es

        if errs.Count = 0 then Ok(List.ofSeq oks) else Error(List.ofSeq errs)

    /// Fails fast on the first error.
    let traverse (f: 'a -> Result<'b, 'e>) (items: 'a list) : Result<'b list, 'e> =
        let rec go acc rest =
            match rest with
            | [] -> Ok(List.rev acc)
            | x :: xs ->
                match f x with
                | Ok v -> go (v :: acc) xs
                | Error e -> Error e

        go [] items

    let sequence (items: Result<'a, 'e> list) = traverse id items

    /// Combines two results, accumulating errors from both sides.
    let zip (a: Result<'a, 'e list>) (b: Result<'b, 'e list>) : Result<'a * 'b, 'e list> =
        match a, b with
        | Ok x, Ok y -> Ok(x, y)
        | Error e1, Error e2 -> Error(e1 @ e2)
        | Error e, _
        | _, Error e -> Error e

    let ofOption err =
        function
        | Some v -> Ok v
        | None -> Error err

    let requireTrue err cond = if cond then Ok() else Error err

    let defaultWith f =
        function
        | Ok v -> v
        | Error e -> f e

/// Computation expression for Result with early return.
type ResultBuilder() =
    member _.Return(x) = Ok x
    member _.ReturnFrom(r: Result<_, _>) = r
    member _.Bind(r, f) = Result.bind f r
    member _.Zero() = Ok()

    member _.Combine(r: Result<unit, 'e>, f: unit -> Result<'a, 'e>) = Result.bind f r

    member _.Delay(f: unit -> Result<_, _>) = f
    member _.Run(f: unit -> Result<_, _>) = f ()

    member this.For(items: 'a seq, body: 'a -> Result<unit, 'e>) : Result<unit, 'e> =
        use e = items.GetEnumerator()
        let mutable state = Ok()

        while (match state with
               | Ok() -> e.MoveNext()
               | Error _ -> false) do
            state <- body e.Current

        state

[<AutoOpen>]
module ResultBuilderInstance =
    let result = ResultBuilder()

[<RequireQualifiedAccess>]
module Map =

    let ofListWithDuplicates (keyOf: 'a -> 'k) (items: 'a list) : Map<'k, 'a> * 'k list =
        let folder (m: Map<'k, 'a>, dups) item =
            let k = keyOf item

            if m.ContainsKey k then (m, k :: dups) else (Map.add k item m, dups)

        let m, dups = List.fold folder (Map.empty, []) items
        m, List.rev dups |> List.distinct

[<RequireQualifiedAccess>]
module String =

    let isBlank (s: string) = String.IsNullOrWhiteSpace s

    let joinWith (sep: string) (items: string seq) = String.Join(sep, items)

    /// Optimal string alignment distance: Levenshtein plus adjacent transpositions, so the
    /// common typo "lable" is one edit from "label". Used to suggest misspelt identifiers.
    let editDistance (a: string) (b: string) =
        let d = Array2D.zeroCreate (a.Length + 1) (b.Length + 1)

        for i in 0 .. a.Length do
            d[i, 0] <- i

        for j in 0 .. b.Length do
            d[0, j] <- j

        for i in 1 .. a.Length do
            for j in 1 .. b.Length do
                let cost = if a[i - 1] = b[j - 1] then 0 else 1
                let best = min (min (d[i - 1, j] + 1) (d[i, j - 1] + 1)) (d[i - 1, j - 1] + cost)

                d[i, j] <-
                    if i > 1 && j > 1 && a[i - 1] = b[j - 2] && a[i - 2] = b[j - 1] then
                        min best (d[i - 2, j - 2] + 1)
                    else
                        best

        d[a.Length, b.Length]

    /// Returns the closest candidate within a small edit distance, if any.
    let suggest (candidates: string seq) (name: string) =
        candidates
        |> Seq.map (fun c -> c, editDistance (invariant c) (invariant name))
        |> Seq.filter (fun (_, d) -> d <= max 1 (name.Length / 3))
        |> Seq.sortBy snd
        |> Seq.tryHead
        |> Option.map fst
