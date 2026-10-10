module Rater.Tests.ParserTests

open System
open Expecto
open Rater
open Rater.Dsl
open Rater.Tests.Support
open Rater.Tests.Generators

let tokens src =
    Lexer.tokenize src |> expectOk |> List.map (fun t -> t.Token)

let syntaxError src =
    match Parser.parseExpression src with
    | Error e -> e
    | Ok e -> failtestf "expected a syntax error for %s but parsed %s" src (Printer.expr e)

let n (d: decimal) = Expr.num d
let r = Expr.ref
let bin op a b = Expr.mk (Binary(op, a, b))

[<Tests>]
let tests =
    testList
        "lexer and parser"
        [ test "numbers accept separators and percent" {
              Expect.equal (tokens "1_000_000") [ TokNumber 1000000m; TokEnd ] "separators"
              Expect.equal (tokens "7.5%") [ TokNumber 0.075m; TokEnd ] "percent"

              Expect.equal
                  (tokens "3.")
                  [ TokNumber 3m; TokDot; TokEnd ]
                  "trailing dot is not part of the number"
          }

          test "strings, dates, tables and comments" {
              Expect.equal
                  (tokens "\"a\\\"b\" @2026-01-31 #base // ignored\nx")
                  [ TokString "a\"b"
                    TokDate(DateOnly(2026, 1, 31))
                    TokTable "base"
                    TokIdent "x"
                    TokEnd ]
                  "tokens"
          }

          test "lexer errors carry positions" {
              let e = Lexer.tokenize "a := 1\nb := \"open" |> expectError
              Expect.equal e.Pos { Line = 2; Column = 6 } "position of the string"
              Expect.stringContains e.Message "unterminated" "message"
          }

          test "single equals gets a helpful message" {
              let e = Lexer.tokenize "a = 1" |> expectError
              Expect.stringContains e.Message "':='" "suggests :="
          }

          test "a number may not run into a name" { Expect.isError (Lexer.tokenize "12abc") "12abc" }

          test "invalid dates are rejected" { Expect.isError (Lexer.tokenize "@2026-02-30") "30 February" }

          test "multiplication binds tighter than addition" {
              Expect.equal (parseExpr "1 + 2 * 3") (bin Add (n 1m) (bin Mul (n 2m) (n 3m))) "precedence"
          }

          test "subtraction is left associative" {
              Expect.equal (parseExpr "10 - 4 - 3") (bin Sub (bin Sub (n 10m) (n 4m)) (n 3m)) "left assoc"
          }

          test "and binds tighter than or, not looser than comparison" {
              let e = parseExpr "not a == b or c and d"

              let expected =
                  bin Or (Expr.mk (Unary(Not, bin Eq (r "a") (r "b")))) (bin And (r "c") (r "d"))

              Expect.equal e expected "boolean precedence"
          }

          test "a minus against a number is a negative literal" {
              Expect.equal (parseExpr "-5") (n -5m) "literal"
              Expect.equal (parseExpr "-x") (Expr.mk (Unary(Negate, r "x"))) "negation"
              Expect.equal (parseExpr "-(5)") (Expr.mk (Unary(Negate, n 5m))) "parenthesised"
          }

          test "dotted references, calls and table lookups" {
              Expect.equal (parseExpr "risk.territory") (Expr.mk (Ref [ "risk"; "territory" ])) "ref"

              Expect.equal
                  (parseExpr "#base[risk.territory, 3]")
                  (Expr.mk (Lookup("base", [ r "risk.territory"; n 3m ])))
                  "lookup"

              Expect.equal (parseExpr "max(1, 2)") (Expr.mk (Call("max", [ n 1m; n 2m ]))) "call"
          }

          test "if with elif chains" {
              let e = parseExpr "if a then 1 elif b then 2 else 3"
              Expect.equal e (Expr.mk (If([ r "a", n 1m; r "b", n 2m ], n 3m))) "if"
          }

          test "if requires else" {
              let e = syntaxError "if a then 1"
              Expect.stringContains e.Message "else" "mentions else"
          }

          test "membership tests" {
              Expect.equal
                  (parseExpr "x in [\"a\", \"b\"]")
                  (Expr.mk (In(r "x", [ Expr.text "a"; Expr.text "b" ])))
                  "in"
          }

          test "comparisons cannot be chained" {
              let e = syntaxError "1 < x < 3"
              Expect.stringContains e.Message "chained" "message"
          }

          test "trailing tokens are reported" {
              let e = syntaxError "1 + 2 )"
              Expect.equal e.Pos.Column 7 "column of the stray parenthesis"
          }

          test "algorithms are sequences of steps without separators" {
              let a = parseAlg "a := 1\nb := a * 2; premium := b + 1"
              Expect.equal (a.Steps |> List.map (fun s -> s.Name)) [ "a"; "b"; "premium" ] "step names"
              Expect.equal a.Steps[2].Pos { Line = 2; Column = 13 } "position of the third step"
          }

          test "steps must use :=" {
              Expect.isError (Parser.parseAlgorithm "a 1") "missing :="
              Expect.isError (Parser.parseAlgorithm "risk.x := 1") "dotted step name"
          }

          test "printer uses the minimum parentheses" {
              let cases =
                  [ "(1 + 2) * 3", "(1 + 2) * 3"
                    "1 + (2 * 3)", "1 + 2 * 3"
                    "a - (b - c)", "a - (b - c)"
                    "(a - b) - c", "a - b - c"
                    "not (a and b)", "not (a and b)"
                    "(not a) and b", "not a and b"
                    "-(a + b)", "-(a + b)"
                    "(a < b) == c", "(a < b) == c"
                    "1 + (if a then 1 else 2)", "1 + (if a then 1 else 2)" ]

              for input, expected in cases do
                  Expect.equal (Printer.expr (parseExpr input)) expected input
          }

          testPropertyWithConfig config "printing then parsing gives back the same tree"
          <| fun (e: Expr) ->
              let printed = Printer.expr e

              match Parser.parseExpression printed with
              | Ok parsed -> parsed = e
              | Error err -> failwithf "could not parse %s: %O" printed err

          testPropertyWithConfig config "printing is stable after one round trip"
          <| fun (e: Expr) ->
              let once = Printer.expr e
              let twice = Printer.expr (parseExpr once)
              once = twice

          testPropertyWithConfig config "the lexer never throws on arbitrary input"
          <| fun (s: string) ->
              match Lexer.tokenize (if isNull s then "" else s) with
              | Ok _
              | Error _ -> true ]
