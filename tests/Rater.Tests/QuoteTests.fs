module Rater.Tests.QuoteTests

open System
open Expecto
open FsCheck
open Rater
open Rater.Tests.Support
open Rater.Tests.Generators

/// A clock the test controls.
type TestClock(start: DateTimeOffset) =
    let mutable now = start
    member _.Now() = now
    member _.Advance(by: TimeSpan) = now <- now + by

let start = DateTimeOffset(2026, 2, 1, 9, 0, 0, TimeSpan.Zero)

let baseRequest = lazy (request (readExample "ho3-request.json"))

let referredRequest =
    lazy { baseRequest.Value with Risk = baseRequest.Value.Risk.Add("roof_age", RawNumber 30m) }

let declinedRequest =
    lazy { baseRequest.Value with Risk = baseRequest.Value.Risk.Add("prior_claims", RawNumber 6m) }

let invalidRequest =
    lazy { baseRequest.Value with Coverages = [] }

let makeDeps (clock: TestClock) =
    let mutable n = 0

    { Rate = fun _ r -> Rating.rate homeowners.Value r
      ValidityDays = fun _ -> 30
      Now = clock.Now
      NextPolicyNumber =
        fun p ->
            n <- n + 1
            $"{p}-{n}" }

/// Runs commands in order, failing the test on the first rejection.
let runAll deps commands =
    commands
    |> List.fold
        (fun (state, events) cmd ->
            match Quote.decide deps state cmd with
            | Ok es -> es |> List.fold (fun s e -> Some(Quote.evolve s e)) state, events @ es
            | Error e -> failtestf "command %A was rejected: %O" cmd e)
        (None, [])

let create (r: RatingRequest) = Create("q-1", "HO3", r)

[<Tests>]
let tests =
    testList
        "quote lifecycle"
        [ test "an accepted quote can be bound" {
              let deps = makeDeps (TestClock start)
              let state, events = runAll deps [ create baseRequest.Value; Rate; Bind ]
              let q = state.Value
              Expect.equal q.Status Bound "bound"
              Expect.equal q.PolicyNumber (Some "HO3-1") "policy number"
              Expect.equal q.Version 3 "three events"
              Expect.equal (events |> List.map (fun e -> e.Name)) [ "created"; "rated"; "bound" ] "event names"
              Expect.equal q.ValidUntil (Some(DateOnly(2026, 3, 3))) "valid for 30 days"
          }

          test "a referral must be approved before binding" {
              let deps = makeDeps (TestClock start)
              let state, _ = runAll deps [ create referredRequest.Value; Rate ]
              Expect.equal state.Value.Status Referred "referred"
              Expect.equal (state.Value.OpenReferrals |> List.map (fun f -> f.RuleId)) [ "UW-ROOF" ] "open referral"

              match Quote.decide deps state Bind with
              | Error(InvalidTransition("bind", "referred")) -> ()
              | other -> failtestf "unexpected %A" other

              let approved, _ = runAll deps [ create referredRequest.Value; Rate; ApproveReferral("UW-ROOF", "dana", "inspection on file") ]
              Expect.equal approved.Value.Status Quoted "quoted after approval"
              Expect.equal approved.Value.Approvals.Head.Underwriter "dana" "approval recorded"
          }

          test "approving an unknown or already-approved referral fails" {
              let deps = makeDeps (TestClock start)
              let state, _ = runAll deps [ create referredRequest.Value; Rate ]

              Expect.equal
                  (Quote.decide deps state (ApproveReferral("UW-NOPE", "dana", "")))
                  (Error(UnknownReferral "UW-NOPE"))
                  "unknown"

              Expect.equal
                  (Quote.decide deps state (ApproveReferral("UW-ROOF", " ", "")))
                  (Error(MissingField "underwriter"))
                  "underwriter required"
          }

          test "declined by rules can be amended and re-rated" {
              let deps = makeDeps (TestClock start)
              let state, _ = runAll deps [ create declinedRequest.Value; Rate ]
              Expect.equal state.Value.Status (Declined ByRules) "declined"
              Expect.isSome state.Value.DeclineReason "reason"

              let fixedUp, _ =
                  runAll deps [ create declinedRequest.Value; Rate; Amend baseRequest.Value; Rate ]

              Expect.equal fixedUp.Value.Status Quoted "quoted after amendment"
          }

          test "an underwriter decline is final" {
              let deps = makeDeps (TestClock start)
              let state, _ = runAll deps [ create referredRequest.Value; Rate; QuoteCommand.Decline("dana", "roof condition") ]
              Expect.equal state.Value.Status (Declined(ByUnderwriter "dana")) "declined"
              Expect.isError (Quote.decide deps state (Amend baseRequest.Value)) "cannot amend"
          }

          test "rating failures are reported and leave the quote in draft" {
              let deps = makeDeps (TestClock start)
              let state, _ = runAll deps [ create invalidRequest.Value ]

              match Quote.decide deps state Rate with
              | Error(RatingFailed issues) -> Expect.exists issues (function MissingRequiredCoverage _ -> true | _ -> false) "issues"
              | other -> failtestf "unexpected %A" other
          }

          test "quotes expire after their validity" {
              let clock = TestClock start
              let deps = makeDeps clock
              let state, _ = runAll deps [ create baseRequest.Value; Rate ]
              Expect.isError (Quote.decide deps state Expire) "still valid"
              clock.Advance(TimeSpan.FromDays 31.0)

              match Quote.decide deps state Bind with
              | Error(QuoteHasExpired _) -> ()
              | other -> failtestf "unexpected %A" other

              let expired = Quote.decide deps state Expire |> expectOk |> List.fold (fun s e -> Some(Quote.evolve s e)) state
              Expect.equal expired.Value.Status Expired "expired"
          }

          test "binding a policy effective in the past is refused" {
              let clock = TestClock start
              let deps = makeDeps clock
              let r = { baseRequest.Value with EffectiveDate = DateOnly(2026, 2, 10) }
              let state, _ = runAll deps [ create r; Rate ]
              clock.Advance(TimeSpan.FromDays 10.0)
              Expect.equal (Quote.decide deps state Bind) (Error(EffectiveDateInPast r.EffectiveDate)) "backdated"
          }

          test "create is rejected for an existing quote and other commands need one" {
              let deps = makeDeps (TestClock start)
              let state, _ = runAll deps [ create baseRequest.Value ]
              Expect.equal (Quote.decide deps state (create baseRequest.Value)) (Error(QuoteAlreadyExists "q-1")) "exists"
              Expect.isError (Quote.decide deps None Rate) "no quote"
          }

          test "evolve rejects malformed histories" {
              Expect.throws (fun () -> Quote.evolve None (QuoteExpired start) |> ignore) "must start with created"
          }

          testPropertyWithConfig quickConfig "replaying the events always rebuilds the same quote"
          <| fun (steps: (byte * byte) list) ->
              let clock = TestClock start
              let deps = makeDeps clock

              let commandFor (kind: byte) (state: Quote option) =
                  match int kind % 9 with
                  | 0 -> Amend baseRequest.Value
                  | 1 -> Amend referredRequest.Value
                  | 2 -> Amend declinedRequest.Value
                  | 3
                  | 4 -> Rate
                  | 5 ->
                      let rule =
                          state
                          |> Option.bind (fun q -> q.OpenReferrals |> List.tryHead)
                          |> Option.map (fun f -> f.RuleId)
                          |> Option.defaultValue "UW-ROOF"

                      ApproveReferral(rule, "uw", "")
                  | 6 -> QuoteCommand.Decline("uw", "appetite")
                  | 7 -> Bind
                  | _ -> Expire

              let mutable state = Some(Quote.evolve None (Created("q", "HO3", baseRequest.Value, clock.Now())))
              let mutable events = [ Created("q", "HO3", baseRequest.Value, clock.Now()) ]
              let mutable ok = true

              for kind, days in steps do
                  clock.Advance(TimeSpan.FromDays(float (int days % 12)))
                  let before = state.Value

                  match Quote.decide deps state (commandFor kind state) with
                  | Ok es ->
                      if before.Status.IsTerminal then ok <- false
                      events <- events @ es
                      state <- es |> List.fold (fun s e -> Some(Quote.evolve s e)) state
                  | Error _ -> ()

              let replayed = Quote.replay events
              let q = state.Value

              ok
              && replayed = state
              && q.Version = events.Length
              && (q.Status <> Bound || q.PolicyNumber.IsSome)
              && (q.Status <> Referred || not q.OpenReferrals.IsEmpty)
              && (q.Status <> Quoted || q.OpenReferrals.IsEmpty)

          testAsync "the store serialises concurrent binds so exactly one wins" {
              let clock = TestClock start
              let store = QuoteStore(makeDeps clock)
              let! created = store.Execute(Create("q-c", "HO3", baseRequest.Value))
              Expect.isOk created "created"
              let! rated = store.Execute(Rate, "q-c")
              Expect.isOk rated "rated"

              let! outcomes = List.init 32 (fun _ -> store.Execute(Bind, "q-c")) |> Async.Parallel
              let wins = outcomes |> Array.filter Result.isOk
              Expect.equal wins.Length 1 "one bind succeeds"

              let! record = store.Get "q-c"
              Expect.equal record.Value.Quote.Status Bound "bound"
              Expect.equal record.Value.Events.Length 3 "no duplicate bind events"
          }

          testAsync "the store enforces expected versions" {
              let store = QuoteStore(makeDeps (TestClock start))
              let! _ = store.Execute(Create("q-v", "HO3", baseRequest.Value))
              let! stale = store.Execute(Rate, "q-v", 5)
              Expect.equal stale (Error(VersionConflict(5, 1))) "conflict"
              let! fresh = store.Execute(Rate, "q-v", 1)
              Expect.isOk fresh "matching version"
              let! missing = store.Execute(Rate, "nope")
              Expect.equal missing (Error(QuoteNotFound "nope")) "missing"
          }

          testAsync "the store expires stale quotes in one sweep" {
              let clock = TestClock start
              let store = QuoteStore(makeDeps clock)

              for id in [ "a"; "b" ] do
                  let! _ = store.Execute(Create(id, "HO3", baseRequest.Value))
                  let! _ = store.Execute(Rate, id)
                  ()

              let! _ = store.Execute(Create("draft", "HO3", baseRequest.Value))
              clock.Advance(TimeSpan.FromDays 40.0)
              let! expired = store.ExpireStale()
              Expect.equal (List.sort expired) [ "a"; "b" ] "only quoted quotes expire"
              let! all = store.List()
              Expect.equal (all |> List.map (fun q -> q.Status.Name) |> List.sort) [ "draft"; "expired"; "expired" ] "statuses"
          } ]
