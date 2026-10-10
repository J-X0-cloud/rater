namespace Rater

open System

/// One filed version of a table, in force from `Effective` until the next version starts.
type TableVersion =
    { Effective: DateOnly
      Label: string option
      Table: Table }

/// A table and its whole filing history. Versions are kept sorted by effective date.
type VersionedTable =
    { Name: string
      Description: string option
      Versions: TableVersion list }

type VersioningError =
    | NoVersions of table: string
    | DuplicateEffectiveDate of table: string * DateOnly
    | SignatureChanged of table: string * effective: DateOnly * detail: string

    override e.ToString() =
        let d = Dates.iso

        match e with
        | NoVersions t -> $"table '{t}' has no versions"
        | DuplicateEffectiveDate(t, date) -> $"table '{t}' has two versions effective {d date}"
        | SignatureChanged(t, date, detail) ->
            $"table '{t}' version effective {d date} changes the table's shape ({detail}); \
              algorithms are checked once against a single shape, so file a new table instead"

/// How one cell moved between two versions of a table.
type CellChange =
    { Keys: DimensionKey list
      Before: decimal option
      After: decimal option }

    /// Relative change; None when the cell was added or removed or was zero before.
    member c.Change =
        match c.Before, c.After with
        | Some b, Some a when b <> 0m -> Some((a - b) / b)
        | _ -> None

type TableDiff =
    { Table: string
      From: DateOnly
      To: DateOnly
      Changed: CellChange list
      Unchanged: int }

    member d.Added = d.Changed |> List.filter (fun c -> c.Before.IsNone)
    member d.Removed = d.Changed |> List.filter (fun c -> c.After.IsNone)

    /// Smallest and largest relative change across cells present in both versions.
    member d.Range =
        let moves = d.Changed |> List.choose (fun c -> c.Change)

        if moves.IsEmpty then None else Some(List.min moves, List.max moves)

[<RequireQualifiedAccess>]
module VersionedTable =

    let private describeSignature (sigs: (string * DimensionKind) list) =
        sigs
        |> List.map (fun (name, kind) ->
            match kind with
            | Exact t -> $"{name}:{t}"
            | Banded -> $"{name}:band"
            | Interpolated _ -> $"{name}:interpolated")
        |> String.joinWith ", "

    /// Sorts versions and checks that the history is well formed: at least one version, no two
    /// sharing a date, and every version with the same dimensions so that an algorithm checked
    /// against one version is valid against all of them.
    let create (name: string) (description: string option) (versions: TableVersion list) =
        let sorted = versions |> List.sortBy (fun v -> v.Effective)

        let errors =
            [ if sorted.IsEmpty then
                  yield NoVersions name

              yield!
                  sorted
                  |> List.countBy (fun v -> v.Effective)
                  |> List.filter (fun (_, n) -> n > 1)
                  |> List.map (fun (d, _) -> DuplicateEffectiveDate(name, d))

              match sorted with
              | first :: rest ->
                  let expected = Table.signature first.Table

                  for v in rest do
                      let actual = Table.signature v.Table

                      if actual <> expected then
                          let detail =
                              $"was ({describeSignature expected}), now ({describeSignature actual})"

                          yield SignatureChanged(name, v.Effective, detail)
              | [] -> () ]

        if errors.IsEmpty then
            Ok
                { Name = name
                  Description = description
                  Versions = sorted }
        else
            Error errors

    /// The version in force on a date: the latest one whose effective date is on or before it.
    let versionAt (date: DateOnly) (t: VersionedTable) =
        t.Versions |> List.takeWhile (fun v -> v.Effective <= date) |> List.tryLast

    let tableAt date t =
        versionAt date t |> Option.map (fun v -> v.Table)

    /// The dimensions shared by every version.
    let dimensions (t: VersionedTable) =
        match t.Versions with
        | v :: _ -> v.Table.Dimensions
        | [] -> []

    /// The first date on which any version is in force.
    let firstEffective (t: VersionedTable) =
        t.Versions |> List.tryHead |> Option.map (fun v -> v.Effective)

    /// Adds a version, keeping the history valid.
    let addVersion (v: TableVersion) (t: VersionedTable) =
        create t.Name t.Description (v :: t.Versions)

    /// Compares the versions in force on two dates cell by cell.
    let diff (fromDate: DateOnly) (toDate: DateOnly) (t: VersionedTable) : Result<TableDiff, string> =
        match versionAt fromDate t, versionAt toDate t with
        | None, _ -> Error $"table '{t.Name}' has no version in force on {Dates.iso fromDate}"
        | _, None -> Error $"table '{t.Name}' has no version in force on {Dates.iso toDate}"
        | Some a, Some b ->
            let cells (v: TableVersion) =
                Table.rows v.Table |> List.map (fun r -> r.Keys, r.Value) |> Map.ofList

            let before = cells a
            let after = cells b

            let keys =
                Seq.append before.Keys after.Keys |> Seq.distinct |> List.ofSeq

            let changes =
                keys
                |> List.map (fun k ->
                    { Keys = k
                      Before = before.TryFind k
                      After = after.TryFind k })

            let changed, same = changes |> List.partition (fun c -> c.Before <> c.After)

            Ok
                { Table = t.Name
                  From = a.Effective
                  To = b.Effective
                  Changed = changed
                  Unchanged = same.Length }
