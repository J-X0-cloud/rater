module Rater.Tests.RateBookTests

open System
open Expecto
open FsCheck
open Rater
open Rater.Tests.Support
open Rater.Tests.Generators

let version (effective: string) (rows: (string * decimal) list) =
    { Effective = date effective
      Label = None
      Table = textTable "t" rows }

let history =
    VersionedTable.create
        "t"
        None
        [ version "2026-01-01" [ "a", 1.1m; "b", 2m ]
          version "2025-01-01" [ "a", 1m; "b", 2m ]
          version "2026-07-01" [ "a", 1.2m; "b", 2m; "c", 3m ] ]
    |> expectOk

[<Tests>]
let tests =
    testList
        "rate book versioning"
        [ test "versions are sorted by effective date" {
              Expect.equal
                  (history.Versions |> List.map (fun v -> Dates.iso v.Effective))
                  [ "2025-01-01"; "2026-01-01"; "2026-07-01" ]
                  "sorted"
          }

          test "the version in force is the latest on or before the date" {
              let at d =
                  VersionedTable.versionAt (date d) history
                  |> Option.map (fun v -> Dates.iso v.Effective)

              Expect.equal (at "2024-12-31") None "before the first version"
              Expect.equal (at "2025-01-01") (Some "2025-01-01") "on the first date"
              Expect.equal (at "2025-12-31") (Some "2025-01-01") "day before the change"
              Expect.equal (at "2026-01-01") (Some "2026-01-01") "on the change"
              Expect.equal (at "2030-01-01") (Some "2026-07-01") "long after"
          }

          test "duplicate effective dates are rejected" {
              let errs =
                  VersionedTable.create
                      "t"
                      None
                      [ version "2025-01-01" [ "a", 1m ]; version "2025-01-01" [ "a", 2m ] ]
                  |> expectError

              Expect.exists
                  errs
                  (function
                  | DuplicateEffectiveDate _ -> true
                  | _ -> false)
                  "duplicate"
          }

          test "a version may not change the table's shape" {
              let numeric =
                  { Effective = date "2026-01-01"
                    Label = None
                    Table = interpolated "t" Clamp [ 1m, 1m ] }

              let errs =
                  VersionedTable.create "t" None [ version "2025-01-01" [ "a", 1m ]; numeric ]
                  |> expectError

              Expect.exists
                  errs
                  (function
                  | SignatureChanged _ -> true
                  | _ -> false)
                  "shape change"
          }

          test "no versions is an error" {
              Expect.isError (VersionedTable.create "t" None []) "empty history"
          }

          test "addVersion keeps the history valid" {
              let added =
                  history
                  |> VersionedTable.addVersion (version "2027-01-01" [ "a", 1.3m ])
                  |> expectOk

              Expect.equal added.Versions.Length 4 "four versions"

              Expect.isError (history |> VersionedTable.addVersion (version "2026-01-01" [ "a", 9m ])) "clash"
          }

          test "diff reports changed, added and unchanged cells" {
              let d =
                  VersionedTable.diff (date "2025-06-01") (date "2026-08-01") history |> expectOk

              Expect.equal d.Unchanged 1 "b is unchanged"
              Expect.equal d.Added.Length 1 "c was added"
              let a = d.Changed |> List.find (fun c -> c.Keys = [ Key(VText "a") ])
              Expect.equal a.Change (Some 0.2m) "a rose by 20%"
              Expect.equal d.Range (Some(0.2m, 0.2m)) "range over cells present in both"
          }

          test "diff fails when no version is in force" {
              Expect.isError (VersionedTable.diff (date "2020-01-01") (date "2026-01-01") history) "too early"
          }

          testPropertyWithConfig config "versionAt picks the latest version not after the date"
          <| fun (dates: Set<DateOnly>) (probe: DateOnly) ->
              let versions =
                  dates
                  |> Set.toList
                  |> List.mapi (fun i d ->
                      { Effective = d
                        Label = None
                        Table = textTable "t" [ "a", decimal i ] })

              match VersionedTable.create "t" None versions with
              | Error _ -> dates.IsEmpty
              | Ok t ->
                  let expected = dates |> Set.filter (fun d -> d <= probe) |> Seq.tryLast

                  (VersionedTable.versionAt probe t |> Option.map (fun v -> v.Effective)) = expected ]
