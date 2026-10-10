module Rater.Tests.ApiTests

open System
open System.Net
open System.Net.Http
open System.Text
open System.Text.Json
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.Logging
open Expecto
open Rater.Api
open Rater.Tests.Support

let private clock () =
    DateTimeOffset(2026, 2, 1, 12, 0, 0, TimeSpan.Zero)

/// Starts an in-memory server over the sample products and hands its client to the test.
let withServer (f: HttpClient -> Async<unit>) =
    async {
        let settings: App.Settings =
            { Catalog = Catalog.ofPlans [ homeowners.Value; auto.Value ]
              Clock = clock
              ExpiryInterval = TimeSpan.Zero }

        let app =
            App.build settings (fun b ->
                b.WebHost.UseTestServer() |> ignore
                b.Logging.SetMinimumLevel(LogLevel.Warning) |> ignore)

        do! app.StartAsync() |> Async.AwaitTask

        try
            use client = app.GetTestClient()
            do! f client
        finally
            app.StopAsync() |> Async.AwaitTask |> Async.RunSynchronously

            (app :> IAsyncDisposable).DisposeAsync().AsTask()
            |> Async.AwaitTask
            |> Async.RunSynchronously
    }

let send
    (client: HttpClient)
    (method: HttpMethod)
    (path: string)
    (body: string option)
    (headers: (string * string) list)
    =
    async {
        use req = new HttpRequestMessage(method, path)

        body
        |> Option.iter (fun b -> req.Content <- new StringContent(b, Encoding.UTF8, "application/json"))

        for k, v in headers do
            req.Headers.TryAddWithoutValidation(k, v) |> ignore

        let! resp = client.SendAsync req |> Async.AwaitTask
        let! text = resp.Content.ReadAsStringAsync() |> Async.AwaitTask

        let doc =
            if String.IsNullOrEmpty text then
                None
            else
                Some(JsonDocument.Parse(text).RootElement.Clone())

        return resp, doc
    }

let get client path = send client HttpMethod.Get path None []

let post client path body =
    send client HttpMethod.Post path body []

let prop (name: string) (e: JsonElement option) = e.Value.GetProperty name

let hoBody = lazy (readExample "ho3-request.json")

let newQuote id (requestJson: string) =
    $$"""{ "id": "{{id}}", "product": "HO3", "request": {{requestJson}} }"""

[<Tests>]
let tests =
    testList
        "http api"
        [ testAsync "health and product listing" {
              do!
                  withServer (fun client ->
                      async {
                          let! resp, body = get client "/health"
                          Expect.equal resp.StatusCode HttpStatusCode.OK "health"
                          Expect.equal ((prop "products" body).GetInt32()) 2 "two products"

                          let! _, products = get client "/products"
                          Expect.equal (products.Value.GetArrayLength()) 2 "listed"

                          let! resp, product = get client "/products/PPA"
                          Expect.equal resp.StatusCode HttpStatusCode.OK "product"
                          Expect.equal ((prop "coverages" product).GetArrayLength()) 4 "coverages"

                          let! resp, _ = get client "/products/NOPE"
                          Expect.equal resp.StatusCode HttpStatusCode.NotFound "unknown product"
                      })
          }

          testAsync "stateless rating returns the worksheet" {
              do!
                  withServer (fun client ->
                      async {
                          let! resp, body = post client "/products/HO3/rate" (Some hoBody.Value)
                          Expect.equal resp.StatusCode HttpStatusCode.OK "rated"
                          Expect.equal ((prop "total" body).GetDecimal()) 2205.51m "total"
                          let dwelling = (prop "coverages" body)[0]
                          Expect.equal (dwelling.GetProperty("worksheet").GetArrayLength()) 10 "ten steps"
                      })
          }

          testAsync "invalid requests are explained" {
              do!
                  withServer (fun client ->
                      async {
                          let! resp, body =
                              post client "/products/HO3/rate" (Some """{ "effectiveDate": "2026-13-01" }""")

                          Expect.equal resp.StatusCode HttpStatusCode.BadRequest "bad json shape"

                          let errors =
                              (prop "errors" body).EnumerateArray()
                              |> Seq.map (fun e -> e.GetString())
                              |> List.ofSeq

                          Expect.exists errors (fun e -> e.Contains "effectiveDate") "date error"

                          Expect.exists
                              errors
                              (fun e -> e.Contains "missing required field 'risk'")
                              "missing risk"

                          let unratable =
                              hoBody.Value.Replace("\"territory\": \"03\"", "\"territory\": \"99\"")

                          let! resp, _ = post client "/products/HO3/rate" (Some unratable)

                          Expect.equal
                              resp.StatusCode
                              HttpStatusCode.UnprocessableEntity
                              "valid shape, invalid risk"
                      })
          }

          testAsync "table versions and diffs" {
              do!
                  withServer (fun client ->
                      async {
                          let! resp, body = get client "/products/HO3/tables/base_rate?date=2025-06-01"
                          Expect.equal resp.StatusCode HttpStatusCode.OK "table"

                          Expect.equal
                              ((prop "version" body).GetProperty("effective").GetString())
                              "2025-01-01"
                              "version"

                          let! resp, body =
                              get client "/products/HO3/tables/base_rate/diff?from=2025-06-01&to=2026-06-01"

                          Expect.equal resp.StatusCode HttpStatusCode.OK "diff"
                          Expect.equal ((prop "changed" body).GetArrayLength()) 15 "every cell moved"

                          let! resp, _ = get client "/products/HO3/tables/base_rate/diff?from=2025-06-01"
                          Expect.equal resp.StatusCode HttpStatusCode.BadRequest "missing to"
                      })
          }

          testAsync "the full quote lifecycle over http" {
              do!
                  withServer (fun client ->
                      async {
                          let! resp, body = post client "/quotes" (Some(newQuote "q-http" hoBody.Value))
                          Expect.equal resp.StatusCode HttpStatusCode.Created "created"
                          Expect.equal (resp.Headers.Location.ToString()) "/quotes/q-http" "location"
                          Expect.equal ((prop "status" body).GetString()) "draft" "draft"

                          let! resp, body = post client "/quotes/q-http/rate" None
                          Expect.equal resp.StatusCode HttpStatusCode.OK "rated"
                          Expect.equal ((prop "status" body).GetString()) "quoted" "quoted"
                          Expect.equal (resp.Headers.ETag.Tag) "\"2\"" "etag carries the version"

                          let! resp, _ =
                              send client HttpMethod.Post "/quotes/q-http/bind" None [ "If-Match", "\"1\"" ]

                          Expect.equal resp.StatusCode HttpStatusCode.PreconditionFailed "stale version"

                          let! resp, body =
                              send client HttpMethod.Post "/quotes/q-http/bind" None [ "If-Match", "\"2\"" ]

                          Expect.equal resp.StatusCode HttpStatusCode.OK "bound"

                          Expect.equal
                              ((prop "policyNumber" body).GetString())
                              "HO3-2026-000001"
                              "policy number"

                          let! resp, _ = post client "/quotes/q-http/rate" None
                          Expect.equal resp.StatusCode HttpStatusCode.Conflict "bound quotes are final"

                          let! _, events = get client "/quotes/q-http/events"

                          Expect.equal
                              (events.Value.EnumerateArray()
                               |> Seq.map (fun e -> e.GetProperty("type").GetString())
                               |> List.ofSeq)
                              [ "created"; "rated"; "bound" ]
                              "event log"
                      })
          }

          testAsync "referral approval and decline over http" {
              do!
                  withServer (fun client ->
                      async {
                          let referred = hoBody.Value.Replace("\"roof_age\": 12", "\"roof_age\": 30")
                          let! _ = post client "/quotes" (Some(newQuote "q-ref" referred))
                          let! _, body = post client "/quotes/q-ref/rate" None
                          Expect.equal ((prop "status" body).GetString()) "referred" "referred"

                          let! resp, _ =
                              post
                                  client
                                  "/quotes/q-ref/referrals/UW-ROOF/approve"
                                  (Some """{ "note": "x" }""")

                          Expect.equal resp.StatusCode HttpStatusCode.BadRequest "underwriter required"

                          let! resp, body =
                              post
                                  client
                                  "/quotes/q-ref/referrals/UW-ROOF/approve"
                                  (Some """{ "underwriter": "dana", "note": "inspected" }""")

                          Expect.equal resp.StatusCode HttpStatusCode.OK "approved"
                          Expect.equal ((prop "status" body).GetString()) "quoted" "quoted"

                          let! resp, body =
                              post
                                  client
                                  "/quotes/q-ref/decline"
                                  (Some """{ "underwriter": "lee", "reason": "changed mind" }""")

                          Expect.equal resp.StatusCode HttpStatusCode.OK "declined"
                          Expect.equal ((prop "declinedBy" body).GetString()) "lee" "declined by"

                          let! _, list = get client "/quotes?status=declined"
                          Expect.equal (list.Value.GetArrayLength()) 1 "filtered list"
                      })
          }

          testAsync "amending a quote returns it to draft" {
              do!
                  withServer (fun client ->
                      async {
                          let! _ = post client "/quotes" (Some(newQuote "q-amend" hoBody.Value))
                          let! _ = post client "/quotes/q-amend/rate" None

                          let! resp, body =
                              send client HttpMethod.Put "/quotes/q-amend/request" (Some hoBody.Value) []

                          Expect.equal resp.StatusCode HttpStatusCode.OK "amended"
                          Expect.equal ((prop "status" body).GetString()) "draft" "back to draft"
                          Expect.equal ((prop "rating" body).ValueKind) JsonValueKind.Null "rating cleared"
                      })
          }

          testAsync "unknown quotes, products and routes are 404s" {
              do!
                  withServer (fun client ->
                      async {
                          let! resp, _ = get client "/quotes/missing"
                          Expect.equal resp.StatusCode HttpStatusCode.NotFound "quote"
                          let! resp, _ = post client "/quotes/missing/rate" None
                          Expect.equal resp.StatusCode HttpStatusCode.NotFound "command on missing quote"

                          let! resp, _ =
                              post client "/quotes" (Some """{ "product": "ZZZ", "request": {} }""")

                          Expect.equal resp.StatusCode HttpStatusCode.BadRequest "invalid request body"
                          let! resp, _ = get client "/nowhere"
                          Expect.equal resp.StatusCode HttpStatusCode.NotFound "route"
                      })
          } ]
