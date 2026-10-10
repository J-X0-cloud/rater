module Rater.Tests.JsonTests

open Expecto
open Rater
open Rater.Json
open Rater.Tests.Support

type Point =
    { X: decimal
      Y: decimal
      Label: string option }

let point: Decoder<Point> =
    Decode.strictObject
        [ "x"; "y"; "label" ]
        (decoder {
            let! x = Decode.field "x" Decode.decimal
            and! y = Decode.field "y" Decode.decimal
            and! label = Decode.optional "label" Decode.string
            return { X = x; Y = y; Label = label }
        })

let messages r = expectError r |> List.map string

let minimalProduct (extra: string) =
    $$"""
    {
      "code": "MIN", "name": "Minimal", "available": "2025-01-01",
      "attributes": [ { "name": "size", "type": "number", "required": true } ],
      "coverages": [ { "code": "A", "name": "A", "limit": { "min": 0, "max": 10, "default": 1 },
                       "algorithm": "premium := #rate[risk.size] * 100" } ],
      "tables": [ {{extra}} ]
    }
    """

[<Tests>]
let tests =
    testList
        "json"
        [ test "applicative decoding accumulates every error with its path" {
              let errs =
                  Decode.fromString (Decode.list point) """[ { "x": 1, "y": 2 }, { "x": "a" } ]"""
                  |> messages

              Expect.contains errs "[1].x: expected a number but found a string" "type error"
              Expect.contains errs "[1]: missing required field 'y'" "missing field"
          }

          test "strict objects reject unknown fields with suggestions" {
              let errs =
                  Decode.fromString point """{ "x": 1, "y": 2, "lable": "p" }""" |> messages

              Expect.contains errs "lable: unknown field 'lable'; did you mean 'label'?" "suggestion"
          }

          test "optional fields treat null as absent" {
              let p = Decode.fromString point """{ "x": 1, "y": 2, "label": null }""" |> expectOk
              Expect.equal p.Label None "null label"
          }

          test "invalid JSON is reported, comments and trailing commas are allowed" {
              Expect.isError (Decode.fromString point "{ x: 1 }") "unquoted name"
              let p = Decode.fromString point "{ \"x\": 1, // c\n \"y\": 2, }" |> expectOk
              Expect.equal p.Y 2m "comment tolerated"
          }

          test "encoders round-trip through text" {
              let node =
                  Encode.object
                      [ "a", Encode.decimal 1.50m
                        "b", Encode.list [ Encode.bool true; Encode.nil ]
                        "c", Encode.date (date "2026-02-03") ]

              Expect.equal
                  (Encode.toCompactString node)
                  """{"a":1.5,"b":[true,null],"c":"2026-02-03"}"""
                  "compact"
          }

          test "band keys parse in all three forms" {
              Expect.equal
                  (ProductCodec.parseBand "1..5" |> expectOk)
                  { Lower = Some 1m; Upper = Some 5m }
                  "closed"

              Expect.equal
                  (ProductCodec.parseBand "5.." |> expectOk)
                  { Lower = Some 5m; Upper = None }
                  "open upper"

              Expect.equal
                  (ProductCodec.parseBand "..0.5" |> expectOk)
                  { Lower = None; Upper = Some 0.5m }
                  "open lower"

              Expect.isError (ProductCodec.parseBand "1-5") "wrong separator"
          }

          test "a table version can derive from the previous one" {
              let json =
                  minimalProduct
                      """{ "name": "rate", "dimensions": [ { "name": "s", "kind": "interpolate", "extrapolation": "clamp" } ],
                           "versions": [ { "effective": "2025-01-01", "rows": [ [0, 1.0], [10, 2.0] ] },
                                         { "effective": "2026-01-01", "derive": { "factor": 1.1, "places": 2 } } ] }"""

              let plan = ProductCodec.load json |> expectOk
              let t = plan.Product.Tables.Head
              let latest = VersionedTable.tableAt (date "2026-06-01") t |> Option.get
              Expect.equal (Table.rows latest |> List.map (fun r -> r.Value)) [ 1.1m; 2.2m ] "scaled cells"
          }

          test "deriving the first version is an error" {
              let json =
                  minimalProduct
                      """{ "name": "rate", "dimensions": [ { "name": "s", "kind": "interpolate" } ],
                           "versions": [ { "effective": "2025-01-01", "derive": { "factor": 1.1 } } ] }"""

              let errs = ProductCodec.load json |> expectError
              Expect.exists errs (fun e -> e.Contains "there is none") "no base version"
          }

          test "syntax errors in algorithms are located" {
              let json =
                  (minimalProduct
                      """{ "name": "rate", "dimensions": [ { "name": "s", "kind": "interpolate" } ],
                           "versions": [ { "effective": "2025-01-01", "rows": [ [0, 1.0] ] } ] }""")
                      .Replace("premium := #rate[risk.size] * 100", "premium := (1 + ")

              let errs = ProductCodec.load json |> expectError

              Expect.exists
                  errs
                  (fun e -> e.StartsWith "coverages[0].algorithm: syntax error at 1:17")
                  "located"
          }

          test "missing table cells are reported per version" {
              let json =
                  minimalProduct
                      """{ "name": "rate", "dimensions": [ { "name": "a" }, { "name": "b" } ],
                           "versions": [ { "effective": "2025-01-01", "rows": [ ["x", "p", 1], ["y", "q", 2] ] } ] }"""

              let errs = ProductCodec.load json |> expectError

              Expect.exists
                  errs
                  (fun e -> e.Contains "version 2025-01-01" && e.Contains "missing 2 cell(s)")
                  "missing cells"
          }

          test "rating requests decode raw values" {
              let r =
                  request
                      """{ "effectiveDate": "2026-01-01", "risk": { "a": 1, "b": "x", "c": true, "d": null },
                           "coverages": [ { "code": "A", "limit": 5 } ] }"""

              Expect.equal r.Risk["a"] (RawNumber 1m) "number"
              Expect.equal r.Risk["d"] RawNull "null"
              Expect.equal r.Coverages.Head.Limit (Some 5m) "limit"
              Expect.isNone r.ExpirationDate "default expiration"
          } ]
