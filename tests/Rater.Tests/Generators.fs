module Rater.Tests.Generators

open System
open FsCheck
open Rater
open Rater.Dsl

/// Decimals with up to six places and a magnitude that keeps products of a few factors finite.
let decimalGen: Gen<decimal> =
    gen {
        let! whole = Gen.choose (-1_000_000, 1_000_000)
        let! frac = Gen.choose (0, 999_999)
        let! places = Gen.choose (0, 6)
        let scaled = decimal frac / pown 10m 6
        let value = decimal whole + (if whole < 0 then -scaled else scaled)
        return Math.Round(value, places)
    }

let positiveDecimalGen =
    decimalGen |> Gen.map abs |> Gen.filter (fun d -> d > 0m)

let dateGen: Gen<DateOnly> =
    Gen.choose (DateOnly(1950, 1, 1).DayNumber, DateOnly(2080, 12, 31).DayNumber)
    |> Gen.map DateOnly.FromDayNumber

let private identGen =
    Gen.elements [ "a"; "b"; "base"; "factor_1"; "x"; "limit"; "territory"; "_tmp" ]

let private textGen =
    Gen.elements [ 'a'; 'Z'; '0'; ' '; '"'; '\\'; '\n'; '\t'; '-'; '#' ]
    |> Gen.listOf
    |> Gen.map (fun cs -> String(Array.ofList cs))

let literalGen: Gen<Value> =
    Gen.oneof
        [ decimalGen |> Gen.map VNumber
          textGen |> Gen.map VText
          Gen.elements [ true; false ] |> Gen.map VBool
          dateGen |> Gen.map VDate ]

let private refGen =
    Gen.oneof
        [ identGen |> Gen.map (fun i -> [ i ])
          Gen.map2 (fun ns f -> [ ns; f ]) (Gen.elements [ "risk"; "coverage"; "policy"; "premium" ]) identGen ]

let private binaryOps =
    [ Add; Sub; Mul; Div; Eq; Ne; Lt; Le; Gt; Ge; And; Or ]

/// Expression trees of the shapes the parser produces. Generated trees are deliberately not
/// well typed: printing and parsing must round-trip regardless of meaning.
let rec exprGen (size: int) : Gen<Expr> =
    let leaf =
        Gen.oneof [ literalGen |> Gen.map Literal; refGen |> Gen.map Ref ]
        |> Gen.map Expr.mk

    if size <= 1 then
        leaf
    else
        let sub = exprGen (size / 2)
        let args =
            gen {
                let! n = Gen.choose (0, 3)
                return! Gen.listOfLength n (exprGen (size / 3))
            }

        Gen.frequency
            [ 3, leaf
              1, Gen.map2 (fun t a -> Expr.mk (Lookup(t, a))) identGen args
              1, Gen.map2 (fun f a -> Expr.mk (Call(f, a))) identGen args
              1, Gen.map2 (fun op x -> Expr.mk (Unary(op, x))) (Gen.elements [ Negate; Not ]) sub
              4, Gen.map3 (fun op l r -> Expr.mk (Binary(op, l, r))) (Gen.elements binaryOps) sub sub
              1,
              gen {
                  let! n = Gen.choose (1, 2)
                  let! branches = Gen.listOfLength n (Gen.zip sub sub)
                  let! otherwise = sub
                  return Expr.mk (If(branches, otherwise))
              }
              1, Gen.map2 (fun x items -> Expr.mk (In(x, items))) sub args ]

type Arbitraries =
    static member Expr() =
        Arb.fromGenShrink (Gen.sized (fun s -> exprGen (min s 40)), fun _ -> Seq.empty)

    static member Date() = Arb.fromGen dateGen

    static member Decimal() =
        Arb.fromGenShrink (decimalGen, fun d -> seq { if d <> 0m then yield Math.Truncate(d / 2m) })

let config =
    { Expecto.FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Arbitraries> ]
        maxTest = 300 }

let quickConfig = { config with maxTest = 100 }
