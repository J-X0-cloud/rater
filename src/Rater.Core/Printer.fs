namespace Rater.Dsl

open System.Globalization
open System.Text
open Rater

/// Renders expressions back to source with the fewest parentheses that preserve meaning.
/// `Parser.parseExpression (Printer.expr e)` yields a tree equal to `e` for every tree the
/// parser itself can produce; the property tests hold the two modules to that contract.
[<RequireQualifiedAccess>]
module Printer =

    let operator =
        function
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
        | Eq -> "=="
        | Ne -> "!="
        | Lt -> "<"
        | Le -> "<="
        | Gt -> ">"
        | Ge -> ">="
        | And -> "and"
        | Or -> "or"

    let private opPower =
        function
        | Or -> 10
        | And -> 20
        | Eq
        | Ne
        | Lt
        | Le
        | Gt
        | Ge -> 40
        | Add
        | Sub -> 50
        | Mul
        | Div -> 60

    let private isComparison op = opPower op = 40

    let private power (e: Expr) =
        match e.Node with
        | If _ -> 0
        | Binary(op, _, _) -> opPower op
        | In _ -> 40
        | Unary(Not, _) -> 30
        | Unary(Negate, _) -> 70
        | Literal(VNumber n) when n < 0m -> 70
        | _ -> 100

    let escape (s: string) =
        let sb = StringBuilder()

        for c in s do
            match c with
            | '"' -> sb.Append("\\\"") |> ignore
            | '\\' -> sb.Append("\\\\") |> ignore
            | '\n' -> sb.Append("\\n") |> ignore
            | '\t' -> sb.Append("\\t") |> ignore
            | c -> sb.Append(c) |> ignore

        sb.ToString()

    let literal (v: Value) =
        match v with
        | VNumber n -> Decimal.toInvariantString n
        | VText s -> $"\"{escape s}\""
        | VBool true -> "true"
        | VBool false -> "false"
        | VDate d -> "@" + d.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture)

    let rec expr (e: Expr) : string =
        let wrap needsParens (child: Expr) =
            let text = expr child
            if needsParens then $"({text})" else text

        match e.Node with
        | Literal v -> literal v
        | Ref path -> String.joinWith "." path
        | Lookup(table, args) -> $"""#{table}[{args |> List.map expr |> String.joinWith ", "}]"""
        | Call(name, args) -> $"""{name}({args |> List.map expr |> String.joinWith ", "})"""
        | Unary(Not, x) -> "not " + wrap (power x < 30) x
        | Unary(Negate, x) ->
            let positiveLiteral =
                match x.Node with
                | Literal(VNumber n) -> n >= 0m
                | _ -> false

            "-" + wrap (positiveLiteral || power x < 70) x
        | Binary(op, l, r) ->
            let p = opPower op
            let leftParens = if isComparison op then power l <= p else power l < p
            $"{wrap leftParens l} {operator op} {wrap (power r <= p) r}"
        | In(x, items) ->
            $"""{wrap (power x <= 40) x} in [{items |> List.map expr |> String.joinWith ", "}]"""
        | If(branches, otherwise) ->
            let sb = StringBuilder()

            branches
            |> List.iteri (fun i (c, v) ->
                let kw = if i = 0 then "if" else " elif"
                sb.Append($"{kw} {expr c} then {expr v}") |> ignore)

            sb.Append($" else {expr otherwise}").ToString()

    let step (s: Step) = $"{s.Name} := {expr s.Expr}"

    let algorithm (a: Algorithm) =
        a.Steps |> List.map step |> String.joinWith "\n"
