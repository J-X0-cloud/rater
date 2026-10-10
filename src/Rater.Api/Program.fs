module Rater.Api.Program

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting

[<EntryPoint>]
let main _ =
    let env name fallback =
        match Environment.GetEnvironmentVariable name with
        | null
        | "" -> fallback
        | v -> v

    let port = env "PORT" "8080"
    let productDir = env "RATER_PRODUCTS_DIR" "products"
    let sweepMinutes = env "RATER_EXPIRY_SWEEP_MINUTES" "15" |> float

    match Catalog.loadDirectory productDir with
    | Error errs ->
        eprintfn "cannot start: the product catalogue is invalid"

        for e in errs do
            eprintfn $"  {e}"

        1
    | Ok catalog ->
        let settings: App.Settings =
            { Catalog = catalog
              Clock = fun () -> DateTimeOffset.UtcNow
              ExpiryInterval = TimeSpan.FromMinutes sweepMinutes }

        let app =
            App.build settings (fun builder -> builder.WebHost.UseUrls($"http://0.0.0.0:{port}") |> ignore)

        app.Run()
        0
