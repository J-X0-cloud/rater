module Rater.Api.App

open System
open System.Threading
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Giraffe
open Rater
open Rater.Api.Handlers

let routes: HttpHandler =
    choose
        [ GET >=> route "/health" >=> health
          subRoute
              "/products"
              (choose
                  [ GET >=> route "" >=> listProducts
                    GET >=> routef "/%s/tables/%s/diff" diffTable
                    GET >=> routef "/%s/tables/%s" getTable
                    POST >=> routef "/%s/rate" rate
                    GET >=> routef "/%s" getProduct ])
          subRoute
              "/quotes"
              (choose
                  [ GET >=> route "" >=> listQuotes
                    POST >=> route "" >=> createQuote
                    POST >=> route "/expire" >=> expireStale
                    GET >=> routef "/%s/events" getEvents
                    PUT >=> routef "/%s/request" amendQuote
                    POST >=> routef "/%s/rate" rateQuote
                    POST >=> routef "/%s/referrals/%s/approve" approveReferral
                    POST >=> routef "/%s/decline" declineQuote
                    POST >=> routef "/%s/bind" bindQuote
                    GET >=> routef "/%s" getQuote ])
          errors 404 [ "no such route" ] ]

/// Periodically expires quotes whose validity has lapsed.
type ExpirySweeper(env: Env, interval: TimeSpan, logger: ILogger<ExpirySweeper>) =
    let mutable timer: Timer option = None

    interface IHostedService with
        member _.StartAsync _ =
            if interval > TimeSpan.Zero then
                let callback _ =
                    let expired = env.Store.ExpireStale() |> Async.RunSynchronously

                    if not expired.IsEmpty then
                        logger.LogInformation("expired {Count} quote(s)", expired.Length)

                timer <- Some(new Timer(callback, null, interval, interval))

            Threading.Tasks.Task.CompletedTask

        member _.StopAsync _ =
            timer |> Option.iter (fun t -> t.Dispose())
            Threading.Tasks.Task.CompletedTask

type Settings =
    { Catalog: Catalog
      Clock: unit -> DateTimeOffset
      ExpiryInterval: TimeSpan }

/// Builds the web application. Tests pass a builder customisation to swap in a test server.
let build (settings: Settings) (customise: WebApplicationBuilder -> unit) : WebApplication =
    let builder = WebApplication.CreateBuilder()
    customise builder

    let env =
        { Catalog = settings.Catalog
          Store = QuoteStore(Lifecycle.deps settings.Catalog settings.Clock)
          Clock = settings.Clock }

    builder.Services.AddGiraffe() |> ignore
    builder.Services.AddSingleton<Env>(env) |> ignore

    builder.Services.AddHostedService<ExpirySweeper>(fun sp ->
        new ExpirySweeper(env, settings.ExpiryInterval, sp.GetRequiredService<ILogger<ExpirySweeper>>()))
    |> ignore

    let app = builder.Build()
    app.UseGiraffeErrorHandler(unhandled) |> ignore
    app.UseGiraffe routes
    app
