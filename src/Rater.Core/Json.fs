namespace Rater.Json

open System
open System.Globalization
open System.Text.Json
open System.Text.Json.Nodes
open Rater

type DecodeError =
    { Path: string
      Message: string }

    override e.ToString() =
        if e.Path = "" then e.Message else $"{e.Path}: {e.Message}"

/// A decoder reads a value at a JSON path, accumulating every error it can find.
type Decoder<'a> = string -> JsonElement -> Result<'a, DecodeError list>

[<RequireQualifiedAccess>]
module Decode =

    let private kind (e: JsonElement) =
        match e.ValueKind with
        | JsonValueKind.Object -> "an object"
        | JsonValueKind.Array -> "an array"
        | JsonValueKind.String -> "a string"
        | JsonValueKind.Number -> "a number"
        | JsonValueKind.True
        | JsonValueKind.False -> "a boolean"
        | JsonValueKind.Null -> "null"
        | _ -> "nothing"

    let fail msg : Decoder<'a> =
        fun path _ -> Error [ { Path = path; Message = msg } ]

    let succeed (x: 'a) : Decoder<'a> = fun _ _ -> Ok x

    let private expected what path (e: JsonElement) =
        Error
            [ { Path = path
                Message = $"expected {what} but found {kind e}" } ]

    let string: Decoder<string> =
        fun path e ->
            match e.ValueKind with
            | JsonValueKind.String -> Ok(e.GetString())
            | _ -> expected "a string" path e

    let decimal: Decoder<decimal> =
        fun path e ->
            match e.ValueKind with
            | JsonValueKind.Number ->
                match e.TryGetDecimal() with
                | true, d -> Ok d
                | _ -> Error [ { Path = path; Message = "number is out of range" } ]
            | _ -> expected "a number" path e

    let int: Decoder<int> =
        fun path e ->
            match e.ValueKind with
            | JsonValueKind.Number ->
                match e.TryGetInt32() with
                | true, i -> Ok i
                | _ -> Error [ { Path = path; Message = "expected a whole number" } ]
            | _ -> expected "a whole number" path e

    let bool: Decoder<bool> =
        fun path e ->
            match e.ValueKind with
            | JsonValueKind.True -> Ok true
            | JsonValueKind.False -> Ok false
            | _ -> expected "a boolean" path e

    let date: Decoder<DateOnly> =
        fun path e ->
            match e.ValueKind with
            | JsonValueKind.String ->
                match Value.parseDate (e.GetString()) with
                | Some d -> Ok d
                | None ->
                    Error
                        [ { Path = path
                            Message = $"'{e.GetString()}' is not a date in yyyy-MM-dd form" } ]
            | _ -> expected "a date string" path e

    let map (f: 'a -> 'b) (d: Decoder<'a>) : Decoder<'b> = fun path e -> d path e |> Result.map f

    let bind (f: 'a -> Decoder<'b>) (d: Decoder<'a>) : Decoder<'b> =
        fun path e ->
            match d path e with
            | Ok v -> f v path e
            | Error errs -> Error errs

    /// Lifts a validation into a decoder, reporting the failure at the current path.
    let andThen (f: 'a -> Result<'b, string>) (d: Decoder<'a>) : Decoder<'b> =
        fun path e ->
            match d path e with
            | Ok v ->
                match f v with
                | Ok b -> Ok b
                | Error msg -> Error [ { Path = path; Message = msg } ]
            | Error errs -> Error errs

    let zip (a: Decoder<'a>) (b: Decoder<'b>) : Decoder<'a * 'b> =
        fun path e -> Result.zip (a path e) (b path e)

    let list (item: Decoder<'a>) : Decoder<'a list> =
        fun path e ->
            match e.ValueKind with
            | JsonValueKind.Array ->
                e.EnumerateArray()
                |> Seq.mapi (fun i x -> i, x)
                |> Result.traverseAll (fun (i, x) -> item $"{path}[{i}]" x)
            | _ -> expected "an array" path e

    let private child path name =
        if path = "" then name else $"{path}.{name}"

    /// A required field.
    let field (name: string) (d: Decoder<'a>) : Decoder<'a> =
        fun path e ->
            match e.ValueKind with
            | JsonValueKind.Object ->
                match e.TryGetProperty name with
                | true, v -> d (child path name) v
                | _ ->
                    Error
                        [ { Path = path
                            Message = $"missing required field '{name}'" } ]
            | _ -> expected "an object" path e

    /// An optional field; absent and null both decode to None.
    let optional (name: string) (d: Decoder<'a>) : Decoder<'a option> =
        fun path e ->
            match e.ValueKind with
            | JsonValueKind.Object ->
                match e.TryGetProperty name with
                | true, v when v.ValueKind <> JsonValueKind.Null -> d (child path name) v |> Result.map Some
                | _ -> Ok None
            | _ -> expected "an object" path e

    let withDefault (fallback: 'a) (name: string) (d: Decoder<'a>) : Decoder<'a> =
        optional name d |> map (Option.defaultValue fallback)

    /// Every property of an object, decoded with the same decoder, in document order.
    let keyValuePairs (d: Decoder<'a>) : Decoder<(string * 'a) list> =
        fun path e ->
            match e.ValueKind with
            | JsonValueKind.Object ->
                e.EnumerateObject()
                |> Result.traverseAll (fun p -> d (child path p.Name) p.Value |> Result.map (fun v -> p.Name, v))
            | _ -> expected "an object" path e

    /// Rejects properties a decoder does not know about, catching misspelt field names.
    let strictObject (allowed: string list) (d: Decoder<'a>) : Decoder<'a> =
        fun path e ->
            let unknown =
                match e.ValueKind with
                | JsonValueKind.Object ->
                    e.EnumerateObject()
                    |> Seq.map (fun p -> p.Name)
                    |> Seq.filter (fun n -> not (List.contains n allowed))
                    |> Seq.map (fun n ->
                        let hint =
                            match String.suggest allowed n with
                            | Some s -> $"; did you mean '{s}'?"
                            | None -> ""

                        { Path = child path n
                          Message = $"unknown field '{n}'{hint}" })
                    |> List.ofSeq
                | _ -> []

            match d path e, unknown with
            | Ok v, [] -> Ok v
            | Ok _, u -> Error u
            | Error errs, u -> Error(errs @ u)

    /// Tries decoders in order, returning the first success.
    let oneOf (decoders: Decoder<'a> list) (what: string) : Decoder<'a> =
        fun path e ->
            decoders
            |> List.tryPick (fun d ->
                match d path e with
                | Ok v -> Some v
                | Error _ -> None)
            |> function
                | Some v -> Ok v
                | None -> expected what path e

    /// The raw element, for callers that need to branch on its shape.
    let element: Decoder<JsonElement> = fun _ e -> Ok e

    let fromString (d: Decoder<'a>) (json: string) : Result<'a, DecodeError list> =
        try
            use doc = JsonDocument.Parse(json, JsonDocumentOptions(CommentHandling = JsonCommentHandling.Skip, AllowTrailingCommas = true))
            d "" (doc.RootElement.Clone())
        with :? JsonException as ex ->
            Error [ { Path = ""; Message = $"invalid JSON: {ex.Message}" } ]

/// Applicative computation expression: `let! ... and! ...` runs every decoder and merges errors.
type DecoderBuilder() =
    member _.Return(x) : Decoder<'a> = Decode.succeed x
    member _.ReturnFrom(d: Decoder<'a>) = d
    member _.BindReturn(d: Decoder<'a>, f: 'a -> 'b) = Decode.map f d
    member _.MergeSources(a: Decoder<'a>, b: Decoder<'b>) = Decode.zip a b
    member _.Bind(d: Decoder<'a>, f: 'a -> Decoder<'b>) = Decode.bind f d

[<AutoOpen>]
module DecoderBuilderInstance =
    let decoder = DecoderBuilder()

[<RequireQualifiedAccess>]
module Encode =

    let string (s: string) : JsonNode = JsonValue.Create s
    let decimal (d: decimal) : JsonNode = JsonValue.Create(Decimal.normalize d)
    let int (i: int) : JsonNode = JsonValue.Create i
    let bool (b: bool) : JsonNode = JsonValue.Create b
    let date (d: DateOnly) : JsonNode = JsonValue.Create(Dates.iso d)

    let dateTime (d: DateTimeOffset) : JsonNode =
        JsonValue.Create(d.ToString("yyyy-MM-ddTHH:mm:ss.fffZ", CultureInfo.InvariantCulture))

    let nil: JsonNode = null

    let option (f: 'a -> JsonNode) =
        function
        | Some v -> f v
        | None -> nil

    let list (items: JsonNode seq) : JsonNode = JsonArray(Array.ofSeq items)

    let object (props: (string * JsonNode) list) : JsonNode =
        let o = JsonObject()

        for k, v in props do
            o[k] <- v

        o

    let value (v: Value) : JsonNode =
        match v with
        | VNumber n -> decimal n
        | VText s -> string s
        | VBool b -> bool b
        | VDate d -> date d

    let private options = JsonSerializerOptions(WriteIndented = true)

    let toString (node: JsonNode) =
        if isNull node then "null" else node.ToJsonString(options)

    let toCompactString (node: JsonNode) =
        if isNull node then "null" else node.ToJsonString()
