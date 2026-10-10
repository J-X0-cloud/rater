module Rater.Tests.TableTests

open Expecto
open FsCheck
open Rater
open Rater.Tests.Support
open Rater.Tests.Generators

let band lo hi = BandKey { Lower = lo; Upper = hi }

let ageTable =
    Table.build
        "age"
        [ { Name = "age"
            Kind = Banded
            Keys = [| band None (Some 5m); band (Some 5m) (Some 15m); band (Some 15m) None |] } ]
        [ { Keys = [ band None (Some 5m) ]; Value = 0.9m }
          { Keys = [ band (Some 5m) (Some 15m) ]; Value = 1m }
          { Keys = [ band (Some 15m) None ]; Value = 1.2m } ]
    |> expectOk

let grid =
    // A 2 x 2 interpolation grid: f(x, y) = x + 10 y at the corners, which bilinear
    // interpolation must reproduce exactly everywhere inside.
    Table.build
        "grid"
        [ { Name = "x"
            Kind = Interpolated Reject
            Keys = [| Point 0m; Point 10m |] }
          { Name = "y"
            Kind = Interpolated Reject
            Keys = [| Point 0m; Point 1m |] } ]
        [ { Keys = [ Point 0m; Point 0m ]; Value = 0m }
          { Keys = [ Point 0m; Point 1m ]; Value = 10m }
          { Keys = [ Point 10m; Point 0m ]; Value = 10m }
          { Keys = [ Point 10m; Point 1m ]; Value = 20m } ]
    |> expectOk

let lookup t args = Table.lookup t args |> expectOk |> fun r -> r.Result

[<Tests>]
let tests =
    testList
        "tables"
        [ test "exact lookup finds a key" {
              let t = textTable "terr" [ "01", 1.1m; "02", 1.2m ]
              Expect.equal (lookup t [ VText "02" ]) 1.2m "row 02"
          }

          test "wildcard catches keys that are not listed" {
              let t =
                  Table.build
                      "t"
                      [ { Name = "k"
                          Kind = Exact TText
                          Keys = [| Key(VText "a"); Wildcard |] } ]
                      [ { Keys = [ Key(VText "a") ]; Value = 1m }; { Keys = [ Wildcard ]; Value = 2m } ]
                  |> expectOk

              Expect.equal (lookup t [ VText "a" ]) 1m "listed key wins"
              Expect.equal (lookup t [ VText "zzz" ]) 2m "wildcard"
          }

          test "missing exact key is an error naming the table and value" {
              let t = textTable "terr" [ "01", 1.1m ]

              match Table.lookup t [ VText "09" ] with
              | Error(NoMatchingKey("terr", "key", VText "09")) -> ()
              | other -> failtestf "unexpected %A" other
          }

          test "argument count and type are checked" {
              let t = textTable "terr" [ "01", 1.1m ]
              Expect.isError (Table.lookup t []) "too few"
              Expect.isError (Table.lookup t [ VNumber 1m ]) "wrong type"
          }

          test "bands are lower-inclusive and upper-exclusive" {
              Expect.equal (lookup ageTable [ VNumber 4.99m ]) 0.9m "below 5"
              Expect.equal (lookup ageTable [ VNumber 5m ]) 1m "5 starts the next band"
              Expect.equal (lookup ageTable [ VNumber -100m ]) 0.9m "open lower bound"
              Expect.equal (lookup ageTable [ VNumber 1000m ]) 1.2m "open upper bound"
          }

          test "a gap between bands is a lookup error" {
              let t =
                  Table.build
                      "gappy"
                      [ { Name = "x"
                          Kind = Banded
                          Keys = [| band (Some 0m) (Some 5m); band (Some 10m) (Some 20m) |] } ]
                      [ { Keys = [ band (Some 0m) (Some 5m) ]; Value = 1m }
                        { Keys = [ band (Some 10m) (Some 20m) ]; Value = 2m } ]
                  |> expectOk

              Expect.isError (Table.lookup t [ VNumber 7m ]) "7 is in the gap"
          }

          test "overlapping and inverted bands are rejected" {
              let errs =
                  Table.build
                      "bad"
                      [ { Name = "x"
                          Kind = Banded
                          Keys = [| band (Some 0m) (Some 10m); band (Some 5m) (Some 20m); band (Some 30m) (Some 25m) |] } ]
                      []
                  |> expectError

              Expect.exists errs (function OverlappingBands(_, _, a, b) -> a <> b | _ -> false) "overlap"
              Expect.exists errs (function OverlappingBands(_, _, a, b) -> a = b | _ -> false) "inverted"
          }

          test "build reports missing and duplicate cells" {
              let dims =
                  [ { Name = "a"
                      Kind = Exact TText
                      Keys = [| Key(VText "x"); Key(VText "y") |] }
                    { Name = "b"
                      Kind = Exact TBool
                      Keys = [| Key(VBool true); Key(VBool false) |] } ]

              let rows =
                  [ { Keys = [ Key(VText "x"); Key(VBool true) ]; Value = 1m }
                    { Keys = [ Key(VText "x"); Key(VBool true) ]; Value = 2m } ]

              let errs = Table.build "t" dims rows |> expectError
              Expect.exists errs (function DuplicateCell _ -> true | _ -> false) "duplicate"
              Expect.exists errs (function MissingCells(_, _, 3) -> true | _ -> false) "three missing"
          }

          test "keys of the wrong type and duplicate dimension names are rejected" {
              let errs =
                  Table.build
                      "t"
                      [ { Name = "a"
                          Kind = Exact TNumber
                          Keys = [| Key(VText "x") |] }
                        { Name = "a"
                          Kind = Exact TNumber
                          Keys = [| Key(VNumber 1m) |] } ]
                      []
                  |> expectError

              Expect.exists errs (function KeyTypeMismatch _ -> true | _ -> false) "type"
              Expect.exists errs (function DuplicateDimension _ -> true | _ -> false) "dimension"
          }

          test "breakpoints must increase" {
              let errs =
                  Table.build
                      "t"
                      [ { Name = "x"
                          Kind = Interpolated Clamp
                          Keys = [| Point 2m; Point 1m |] } ]
                      []
                  |> expectError

              Expect.exists errs (function UnsortedBreakpoints _ -> true | _ -> false) "unsorted"
          }

          test "linear interpolation between breakpoints" {
              let t = interpolated "aoi" Reject [ 100m, 1m; 200m, 2m; 400m, 3m ]
              Expect.equal (lookup t [ VNumber 150m ]) 1.5m "midpoint"
              Expect.equal (lookup t [ VNumber 300m ]) 2.5m "second segment"
              Expect.equal (lookup t [ VNumber 400m ]) 3m "last point"
          }

          test "extrapolation modes" {
              let pts = [ 10m, 1m; 20m, 2m ]
              Expect.isError (Table.lookup (interpolated "r" Reject pts) [ VNumber 25m ]) "reject"
              Expect.equal (lookup (interpolated "c" Clamp pts) [ VNumber 25m ]) 2m "clamp high"
              Expect.equal (lookup (interpolated "c" Clamp pts) [ VNumber 0m ]) 1m "clamp low"
              Expect.equal (lookup (interpolated "l" Linear pts) [ VNumber 25m ]) 2.5m "linear high"
              Expect.equal (lookup (interpolated "l" Linear pts) [ VNumber 0m ]) 0m "linear low"
          }

          test "a single breakpoint behaves as a constant only when clamping" {
              Expect.equal (lookup (interpolated "c" Clamp [ 5m, 7m ]) [ VNumber 99m ]) 7m "clamped"
              Expect.isError (Table.lookup (interpolated "r" Reject [ 5m, 7m ]) [ VNumber 99m ]) "rejected"
          }

          test "bilinear interpolation reproduces a plane" {
              Expect.equal (lookup grid [ VNumber 5m; VNumber 0.5m ]) 10m "centre"
              Expect.equal (lookup grid [ VNumber 2.5m; VNumber 1m ]) 12.5m "edge"
          }

          testPropertyWithConfig config "bilinear interpolation is exact for f(x, y) = x + 10y"
          <| fun (a: uint16) (b: uint16) ->
              let x = decimal (int a % 1001) / 100m
              let y = decimal (int b % 101) / 100m
              lookup grid [ VNumber x; VNumber y ] = x + 10m * y

          testPropertyWithConfig config "interpolated values lie between the neighbouring cells"
          <| fun (raw: NonEmptyArray<int16>) (probe: uint16) ->
              let values = raw.Get |> Array.map decimal
              let points = values |> Array.mapi (fun i v -> decimal (i * 10), v) |> List.ofArray
              let t = interpolated "p" Clamp points
              let x = decimal (int probe % (values.Length * 10))
              let result = lookup t [ VNumber x ]
              let i = min (int (x / 10m)) (values.Length - 1)
              let j = min (i + 1) (values.Length - 1)
              result >= min values[i] values[j] && result <= max values[i] values[j]

          testPropertyWithConfig config "band lookup agrees with a linear scan"
          <| fun (cuts: Set<int16>) (probe: int16) ->
              let edges = cuts |> Set.toList |> List.map decimal

              let bands =
                  match edges with
                  | [] -> [ { Lower = None; Upper = None } ]
                  | _ ->
                      ({ Lower = None; Upper = Some edges.Head }
                       :: (edges |> List.pairwise |> List.map (fun (a, b) -> { Lower = Some a; Upper = Some b })))
                      @ [ { Lower = Some(List.last edges); Upper = None } ]

              let t =
                  Table.build
                      "b"
                      [ { Name = "x"
                          Kind = Banded
                          Keys = bands |> List.map BandKey |> Array.ofList } ]
                      (bands |> List.mapi (fun i b -> { Keys = [ BandKey b ]; Value = decimal i }))
                  |> expectOk

              let x = decimal probe
              let expected = bands |> List.findIndex (fun b -> b.Contains x) |> decimal
              lookup t [ VNumber x ] = expected

          test "scale applies a factor and rounds every cell" {
              let t = textTable "s" [ "a", 100m; "b", 333m ] |> Table.scale 1.045m 0
              Expect.equal (Table.rows t |> List.map (fun r -> r.Value)) [ 105m; 348m ] "scaled"
          }

          test "lookup trace explains each dimension" {
              let t = interpolated "aoi" Reject [ 100m, 1m; 200m, 2m ]
              let trace = Table.lookup t [ VNumber 125m ] |> expectOk
              Expect.equal trace.Explanation [ "x: between 100 and 200 (weight 0.25)" ] "explanation"
          } ]
