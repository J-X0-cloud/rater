module Rater.Tests.TypeCheckTests

open Expecto
open Rater
open Rater.Dsl
open Rater.Tests.Support

let env =
    { Attributes =
        Map.ofList
            [ "territory", TText
              "year_built", TNumber
              "has_alarm", TBool
              "birth_date", TDate ]
      AllowCoverage = true
      PremiumRefs = set [ "DWELLING" ]
      Tables =
        Map.ofList
            [ "base",
              [ { Name = "territory"
                  Kind = Exact TText
                  Keys = [||] }
                { Name = "limit"
                  Kind = Interpolated Reject
                  Keys = [||] } ] ] }

let typeOf src =
    TypeCheck.expression env (parseExpr src) |> expectOk |> fst

let errorsOf src =
    TypeCheck.expression env (parseExpr src)
    |> expectError
    |> List.map (fun d -> d.Message)

let algorithmErrors src =
    TypeCheck.algorithm env (parseAlg src)
    |> expectError
    |> List.map (fun d -> d.Message)

let algorithmWarnings src =
    (TypeCheck.algorithm env (parseAlg src) |> expectOk).Warnings
    |> List.map (fun d -> d.Message)

[<Tests>]
let tests =
    testList
        "type checker"
        [ test "infers types of references, lookups and calls" {
              Expect.equal (typeOf "risk.territory") TText "attribute"
              Expect.equal (typeOf "#base[risk.territory, coverage.limit]") TNumber "lookup"
              Expect.equal (typeOf "years_between(risk.birth_date, policy.effective_date)") TNumber "call"
              Expect.equal (typeOf "risk.has_alarm and policy.renewal") TBool "and"
              Expect.equal (typeOf "premium.DWELLING * 2") TNumber "premium reference"
              Expect.equal (typeOf "risk.territory in [\"01\", \"02\"]") TBool "membership"
          }

          test "unknown names come with suggestions" {
              let errs = errorsOf "risk.teritory"
              Expect.exists errs (fun m -> m.Contains "did you mean 'territory'") "suggestion"
          }

          test "unknown tables and functions are reported" {
              Expect.exists (errorsOf "#bse[1]") (fun m -> m.Contains "unknown table 'bse'") "table"
              Expect.exists (errorsOf "maxx(1, 2)") (fun m -> m.Contains "did you mean 'max'") "function"
          }

          test "table arity and argument types are checked" {
              Expect.exists
                  (errorsOf "#base[risk.territory]")
                  (fun m -> m.Contains "takes 2 argument")
                  "arity"

              Expect.exists
                  (errorsOf "#base[1, coverage.limit]")
                  (fun m -> m.Contains "dimension 'territory' expects a text")
                  "argument type"
          }

          test "operator type errors" {
              Expect.exists (errorsOf "risk.territory + 1") (fun m -> m.Contains "needs two numbers") "add"

              Expect.exists
                  (errorsOf "risk.territory < \"b\"")
                  (fun m -> m.Contains "orders numbers and dates")
                  "order"

              Expect.exists (errorsOf "1 == true") (fun m -> m.Contains "cannot compare") "eq"
              Expect.exists (errorsOf "not 1") (fun m -> m.Contains "'not' expects a bool") "not"
              Expect.exists (errorsOf "-risk.has_alarm") (fun m -> m.Contains "cannot negate") "negate"
              Expect.exists (errorsOf "1 / 0") (fun m -> m.Contains "division by zero") "literal zero"
          }

          test "if branches must agree and conditions must be bool" {
              Expect.exists
                  (errorsOf "if true then 1 else \"x\"")
                  (fun m -> m.Contains "branches must agree")
                  "branches"

              Expect.exists (errorsOf "if 1 then 1 else 2") (fun m -> m.Contains "must be a bool") "condition"
          }

          test "membership lists must match the tested type" {
              Expect.exists
                  (errorsOf "risk.year_built in [1, \"2\"]")
                  (fun m -> m.Contains "'in' list holds a text")
                  "in"
          }

          test "builtin overloads are resolved" {
              Expect.equal (typeOf "round(1.234)") TNumber "one argument"
              Expect.equal (typeOf "round(1.234, 2)") TNumber "two arguments"
              Expect.exists (errorsOf "round(\"x\")") (fun m -> m.Contains "does not match") "bad overload"
              Expect.exists (errorsOf "min(1)") (fun m -> m.Contains "at least 2") "variadic minimum"
          }

          test "coverage fields are unavailable outside algorithms" {
              let ruleEnv = { env with AllowCoverage = false }

              let errs =
                  TypeCheck.expression ruleEnv (parseExpr "coverage.limit > 1") |> expectError

              Expect.exists errs (fun d -> d.Message.Contains "only available inside") "coverage in rule"
          }

          test "steps see only earlier steps" {
              Expect.exists
                  (algorithmErrors "a := b\nb := 1\npremium := a")
                  (fun m -> m.Contains "unknown name 'b'")
                  "forward"
          }

          test "algorithms must define a numeric premium" {
              Expect.exists
                  (algorithmErrors "a := 1")
                  (fun m -> m.Contains "must define a 'premium'")
                  "missing"

              Expect.exists
                  (algorithmErrors "premium := true")
                  (fun m -> m.Contains "must be a number")
                  "bool"
          }

          test "duplicate and reserved step names are errors" {
              Expect.exists
                  (algorithmErrors "a := 1\na := 2\npremium := a")
                  (fun m -> m.Contains "more than once")
                  "dup"

              Expect.exists
                  (algorithmErrors "risk := 1\npremium := 1")
                  (fun m -> m.Contains "reserved")
                  "reserved"

              Expect.exists
                  (algorithmErrors "max := 1\npremium := 1")
                  (fun m -> m.Contains "built-in")
                  "builtin"
          }

          test "unused steps and steps after premium produce warnings" {
              let ws = algorithmWarnings "a := 1\nb := 2\npremium := a\nc := 3"
              Expect.exists ws (fun m -> m.Contains "'b' is never used") "unused"
              Expect.exists ws (fun m -> m.Contains "'c' comes after 'premium'") "after premium"
          }

          test "errors are positioned" {
              let errs =
                  TypeCheck.algorithm env (parseAlg "a := 1\npremium := a + risk.nope")
                  |> expectError

              Expect.equal errs.Head.Pos { Line = 2; Column = 16 } "position of the bad reference"
          } ]
