# rater

An insurance rating and quoting engine written in F# on .NET 8.

Products are data. A product file declares the risk attributes it rates on, its coverages,
its rate tables with their full filing history, its underwriting rules and, for each coverage,
a rating algorithm written in a small typed expression language. The engine validates the
whole product up front, compiles the algorithms to closures, and then rates requests into a
step-by-step worksheet that shows every factor and every table lookup. Quotes move through an
event-sourced lifecycle (draft, quoted, referred, declined, bound, expired) behind a Giraffe
HTTP API and a command-line tool.

## Features

- **Rating language.** Steps of the form `name := expression` with numbers (exact decimals),
  text, booleans and dates; `if / elif / else`; `in [...]` membership; table lookups written
  `#table[arg, ...]`; references to `risk.*`, `coverage.*`, `policy.*` and to the premium of
  another coverage (`premium.DWELLING`); and built-ins for rounding (four modes, negative
  places), `min`/`max`/`clamp`, whole-number `pow`, and date arithmetic. `7.5%` is 0.075.
- **Static checking before any quote is rated.** A type checker resolves every reference,
  checks table arity and argument types against the table's dimensions, checks operator and
  built-in overloads, and rejects forward references, duplicate steps and a missing or
  non-numeric `premium`. Errors carry line and column positions and "did you mean" suggestions
  (optimal string alignment distance, so `lable` suggests `label`). Unused steps and steps after
  `premium` are reported as warnings.
- **Rate tables.** Dense N-dimensional tables with three kinds of dimension: exact keys (with an
  optional `*` wildcard), numeric bands (`"5..15"`, lower bound inclusive, open ends allowed) and
  interpolated breakpoints. Interpolation is multilinear across any number of interpolated
  dimensions, with clamp, linear or reject extrapolation. Tables are checked for missing cells,
  duplicate cells, overlapping bands and unsorted breakpoints when they are loaded.
- **Versioning by effective date.** Each table keeps its filing history. The version in force on
  the policy effective date prices the quote and is recorded on the result for reproducibility.
  A version can be filed as explicit rows or derived from the previous one (`"derive": {"factor":
  1.045, "places": 0}`). Every version must keep the table's shape, so an algorithm checked once
  is valid against all of them. Versions can be diffed cell by cell.
- **Premium rules.** Coverage minimum premiums, configurable rounding, term pro-rating (whole
  years count as one each, so an exact annual term over 29 February is still 1.0), a policy
  minimum premium whose uplift is spread across coverages with the largest-remainder method so
  the parts add back exactly, flat fees, and percentage taxes optionally levied on fees.
- **Coverage dependencies.** Coverages that reference another coverage's premium are rated
  after it (Kahn's algorithm, ties broken by declaration order); cycles and self references
  are product errors. Selecting a coverage without the coverages it depends on is a request
  error.
- **Underwriting rules.** Boolean conditions in the same language, with `refer` or `decline`
  actions. Rules see the risk, the policy dates and every coverage premium plus `premium.total`.
  A rule that fails to evaluate refers the quote instead of passing silently.
- **Quote lifecycle.** A pure `decide`/`evolve` state machine over events. Referrals must each be
  approved by a named underwriter before binding; amendments return a quote to draft; quotes
  expire after the product's validity period; binding is refused for back-dated policies.
  An in-memory store serialises commands through a mailbox and supports optimistic
  concurrency, which the HTTP API exposes through `ETag` and `If-Match`.
- **Errors you can act on.** JSON decoding is applicative and accumulates every error with its
  path; unknown fields are rejected with suggestions; product validation reports every problem
  in one pass; request validation reports every invalid attribute and coverage at once.

## Architecture

```
src/
  Rater.Core/            the engine, no web dependencies
    Prelude.fs           Result helpers, edit distance
    Money.fs             rounding modes, Money, largest-remainder allocation
    Value.fs             runtime values and types, date arithmetic
    Tables.fs            dense tables, dimension indexes, multilinear lookup
    Ast.fs Lexer.fs      rating language syntax tree and tokenizer
    Parser.fs            Pratt parser
    Printer.fs           minimal-parenthesis pretty printer (inverse of the parser)
    Builtins.fs          built-in functions and their signatures
    TypeCheck.fs         static checker
    Interpreter.fs       compiles expressions to closures, runs algorithms into worksheets
    RateBook.fs          versioned tables, effective-date selection, diffs
    Product.fs           product model, validation, dependency ordering -> RatingPlan
    Underwriting.fs      rule evaluation and decisions
    Rating.fs            request validation and the rating pipeline
    Quote.fs             lifecycle state machine
    QuoteStore.fs        mailbox-serialised event store
    Json.fs              decoder combinators and encoders over System.Text.Json
    ProductCodec.fs      product files
    ModelCodec.fs        requests, results, quotes, diffs
  Rater.Api/             Giraffe service
  Rater.Cli/             `rater` command-line tool and benchmark harness
tests/Rater.Tests/       Expecto + FsCheck
products/                sample products (homeowners, personal auto)
examples/                sample requests and a small book of business
```

Rating a request runs in this order: check the term and product availability, validate the
risk against the attribute schema (coercing text such as `"yes"` or `"6"` where the type
allows), resolve coverage selections to limits and deductibles, pick the table versions in
force, run each coverage's compiled algorithm in dependency order, apply minimums, rounding,
pro-rating, the policy minimum, fees and taxes, then evaluate underwriting rules.

## The rating language

```
// Dwelling (Coverage A) from the homeowners sample
base       := #base_rate[risk.territory, risk.construction]
amount     := #amount_of_insurance[coverage.limit]
home_age   := #home_age[year(policy.effective_date) - risk.year_built]
claims     := #claims_surcharge[min(risk.prior_claims, 3)]
roof       := if risk.roof_age >= 20 then 1.15 elif risk.roof_age >= 15 then 1.05 else 1
protective := 1 - min(15%, (if risk.has_alarm then 5% else 0) + (if risk.sprinklered then 10% else 0))
premium    := round(base * amount * home_age * claims * roof * protective, 2)
```

| Reference | Type |
| --- | --- |
| `risk.<attribute>` | as declared in the product |
| `coverage.limit`, `coverage.deductible` | number (algorithms only) |
| `coverage.code` | text (algorithms only) |
| `policy.effective_date`, `policy.expiration_date` | date |
| `policy.term_days` | number |
| `policy.renewal` | bool |
| `premium.<COVERAGE>` | number; in rules also `premium.total` |
| `<step>` | any earlier step |

Operators, loosest first: `or`, `and`, `not`, comparisons and `in` (comparisons do not chain),
`+ -`, `* /`, unary `-`. Literals: `1_000`, `2.5`, `7.5%`, `"text"`, `true`, `@2026-01-31`.
Comments start with `//`.

Built-ins: `min`, `max` (two or more numbers), `clamp`, `abs`, `floor`, `ceil`, `pow` (whole
exponent), `round`, `round_even`, `round_up`, `round_down` (optional places, negative allowed),
`years_between`, `months_between`, `days_between`, `year`, `month`, `add_years`, `lower`,
`upper`, `starts_with`, `length`.

## Product files

A trimmed example; see `products/homeowners.json` and `products/personal-auto.json` for
complete ones.

```json
{
  "code": "HO3",
  "name": "Homeowners Special Form",
  "available": "2025-01-01",
  "attributes": [
    { "name": "territory", "type": "text", "required": true, "allowed": ["01", "02"] },
    { "name": "year_built", "type": "number", "required": true, "integer": true, "min": 1800 }
  ],
  "coverages": [
    {
      "code": "DWELLING", "name": "Coverage A", "required": true,
      "limit": { "min": 100000, "max": 2000000, "increment": 1000 },
      "deductibles": [500, 1000], "defaultDeductible": 1000, "minimumPremium": 150,
      "algorithm": [
        "base := #base_rate[risk.territory]",
        "premium := round(base * #amount[coverage.limit], 2)"
      ]
    }
  ],
  "tables": [
    {
      "name": "base_rate",
      "dimensions": [ { "name": "territory" } ],
      "versions": [
        { "effective": "2025-01-01", "rows": [ ["01", 812], ["02", 905] ] },
        { "effective": "2026-01-01", "label": "+4.5%", "derive": { "factor": 1.045, "places": 0 } }
      ]
    },
    {
      "name": "amount",
      "dimensions": [ { "name": "limit", "kind": "interpolate", "extrapolation": "reject" } ],
      "versions": [ { "effective": "2025-01-01", "rows": [ [100000, 0.62], [2000000, 6.1] ] } ]
    }
  ],
  "rules": [
    { "id": "UW-AGE", "when": "year(policy.effective_date) - risk.year_built > 120",
      "action": "refer", "message": "dwelling over 120 years old" }
  ],
  "fees": [ { "code": "POLICY_FEE", "name": "Policy fee", "amount": 25 } ],
  "taxes": [ { "code": "PREMIUM_TAX", "name": "Premium tax", "rate": 0.023 } ],
  "premium": { "rounding": "half_up", "places": 0, "policyMinimum": 300 }
}
```

Dimension kinds are `exact` (default; `type` is `text`, `number`, `bool` or `date`), `band` and
`interpolate`. Table rows are arrays of keys followed by the value.

## Quick start

Requires the .NET 8 SDK.

```bash
dotnet tool restore
dotnet build

# validate products, then rate a request and print its worksheet
dotnet run --project src/Rater.Cli -- validate products/*.json
dotnet run --project src/Rater.Cli -- rate products/homeowners.json examples/ho3-request.json

# the same result as JSON, or at another effective date
dotnet run --project src/Rater.Cli -- rate products/homeowners.json examples/ho3-request.json --json
dotnet run --project src/Rater.Cli -- rate products/homeowners.json examples/ho3-request.json --date 2025-06-01

# filing history
dotnet run --project src/Rater.Cli -- tables products/personal-auto.json --date 2026-05-01
dotnet run --project src/Rater.Cli -- diff products/personal-auto.json territory_liability 2025-08-01 2026-06-01
dotnet run --project src/Rater.Cli -- impact products/personal-auto.json examples/ppa-book.json --from 2026-01-15 --to 2026-05-01
```

### HTTP API

```bash
PORT=8080 RATER_PRODUCTS_DIR=products dotnet run --project src/Rater.Api
```

| Method | Path | Purpose |
| --- | --- | --- |
| GET | `/health` | liveness and product count |
| GET | `/products`, `/products/{code}` | catalogue and product detail (algorithms printed back) |
| GET | `/products/{code}/tables/{name}?date=` | table version in force on a date |
| GET | `/products/{code}/tables/{name}/diff?from=&to=` | cell-by-cell comparison |
| POST | `/products/{code}/rate` | stateless rating with worksheet |
| POST | `/quotes` | create a draft (`{ "id"?, "product", "request" }`) |
| GET | `/quotes?status=`, `/quotes/{id}`, `/quotes/{id}/events` | read quotes and their event log |
| PUT | `/quotes/{id}/request` | amend; returns the quote to draft |
| POST | `/quotes/{id}/rate` | rate a draft |
| POST | `/quotes/{id}/referrals/{rule}/approve` | `{ "underwriter", "note" }` |
| POST | `/quotes/{id}/decline` | `{ "underwriter", "reason" }` |
| POST | `/quotes/{id}/bind` | bind a quoted quote |
| POST | `/quotes/expire` | expire lapsed quotes now (also runs on a timer) |

Quote responses carry `ETag: "<version>"`; send `If-Match` on a command to have it refused with
412 if the quote changed in the meantime. Validation failures are 400 (malformed body) or 422
(well formed but not ratable), illegal transitions 409.

Environment: `PORT` (default 8080), `RATER_PRODUCTS_DIR` (default `products`),
`RATER_EXPIRY_SWEEP_MINUTES` (default 15, 0 disables the timer). The service refuses to start if
any product file is invalid.

### Docker

```bash
docker build -t rater-api .
docker run -e PORT=8080 -p 8080:8080 rater-api
```

## Running the tests

```bash
dotnet test                                  # through the VSTest adapter
dotnet run --project tests/Rater.Tests       # Expecto runner directly (supports --filter etc.)
dotnet fantomas --check src tests            # formatting
```

180 tests cover the money, date, table, parser, type checker, interpreter, versioning, product
validation, JSON, rating, lifecycle, store and HTTP layers. FsCheck properties include: printing
then parsing any expression tree returns the same tree; bilinear interpolation reproduces a
plane exactly; band lookup agrees with a linear scan; allocation always sums to the rounded
total with each part within one cent of its exact share; `versionAt` picks the latest version
not after the date; dependency ordering respects every edge; dwelling premium never falls as the
limit rises; replaying a quote's events always rebuilds the same quote, terminal quotes never
accept commands and bound quotes always have a policy number. A concurrency test fires 32
parallel binds at one quote and checks that exactly one succeeds.

## Benchmarks

Measured with `rater bench` (Release build, a one-second warm-up, then 20,000 ratings) on an
AMD Ryzen 7 7800X3D under WSL2. The machine was shared and heavily loaded during the runs (load
average 35 to 38), so treat these as upper bounds; the ranges are three consecutive runs.

| Benchmark | Homeowners (5 coverages, 10 tables) | Personal auto (4 coverages, 11 tables) |
| --- | --- | --- |
| Load, validate and compile the product | 3.0 to 5.4 ms | 3.5 to 4.1 ms |
| Rate one request with full worksheet | 47 to 55 us (43.5 KiB allocated) | 43 to 121 us (54.5 KiB allocated) |
| Create and rate a quote through `Quote.decide` | 49 to 57 us | 49 to 69 us |

## Roadmap

Things that are deliberately not done yet:

- Persistence. The quote store is in memory; quotes do not survive a restart. The store is a
  thin layer over `Quote.decide`/`Quote.evolve`, so an event table in a database is the natural
  next step.
- Algorithm versioning. Tables are versioned by effective date, but a coverage has a single
  algorithm. Changing the structure of a calculation currently means a new product version.
- Multi-risk policies (several vehicles or locations on one quote) and policy-level discounts
  that span risks.
- Mid-term changes, cancellations and short-rate refunds; only new business and renewal quotes
  are modelled.
- Authority levels for underwriters. Any named underwriter can approve any referral.
- Authentication and authorisation on the HTTP API.
- A statistical benchmark harness; `rater bench` is a simple stopwatch loop.

## License

MIT, see [LICENSE](LICENSE).
