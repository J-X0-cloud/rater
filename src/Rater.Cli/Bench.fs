module Rater.Cli.Bench

open System
open System.Diagnostics
open Rater

type Measurement =
    { Name: string
      Iterations: int
      Elapsed: TimeSpan
      AllocatedBytes: int64 }

    member m.MicrosecondsPerOp = m.Elapsed.TotalMilliseconds * 1000.0 / float m.Iterations
    member m.OpsPerSecond = float m.Iterations / m.Elapsed.TotalSeconds
    member m.BytesPerOp = float m.AllocatedBytes / float m.Iterations

/// Runs `f` for a warm-up period, then times `iterations` calls. Good enough to compare
/// changes on one machine; not a substitute for a statistical harness.
let measure (name: string) (iterations: int) (f: unit -> unit) =
    let warmup = max 1 (iterations / 10)

    for _ in 1..warmup do
        f ()

    GC.Collect()
    GC.WaitForPendingFinalizers()
    GC.Collect()

    let allocatedBefore = GC.GetAllocatedBytesForCurrentThread()
    let sw = Stopwatch.StartNew()

    for _ in 1..iterations do
        f ()

    sw.Stop()

    { Name = name
      Iterations = iterations
      Elapsed = sw.Elapsed
      AllocatedBytes = GC.GetAllocatedBytesForCurrentThread() - allocatedBefore }

let report (ms: Measurement list) =
    let rows =
        ms
        |> List.map (fun m ->
            [ m.Name
              string m.Iterations
              m.MicrosecondsPerOp.ToString("0.0", Globalization.CultureInfo.InvariantCulture)
              m.OpsPerSecond.ToString("#,##0", Globalization.CultureInfo.InvariantCulture)
              (m.BytesPerOp / 1024.0).ToString("0.0", Globalization.CultureInfo.InvariantCulture) ])

    Render.table [ "Benchmark"; "Iterations"; "us/op"; "ops/s"; "KiB/op" ] rows
