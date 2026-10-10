namespace Rater.Dsl

open System
open Rater

/// A source position, 1-based.
[<Struct>]
type Pos =
    { Line: int
      Column: int }

    override p.ToString() = $"{p.Line}:{p.Column}"

    static member Start = { Line = 1; Column = 1 }

type UnaryOp =
    | Negate
    | Not

type BinaryOp =
    | Add
    | Sub
    | Mul
    | Div
    | Eq
    | Ne
    | Lt
    | Le
    | Gt
    | Ge
    | And
    | Or

/// An expression node with its source position. Equality and hashing ignore positions so that
/// trees built by hand compare equal to trees produced by the parser.
[<CustomEquality; NoComparison>]
type Expr =
    { Node: Node
      Pos: Pos }

    override e.Equals(o) =
        match o with
        | :? Expr as other -> e.Node = other.Node
        | _ -> false

    override e.GetHashCode() = e.Node.GetHashCode()

and [<CustomEquality; NoComparison>] Node =
    /// A constant: number, text, bool or date.
    | Literal of Value
    /// A dotted reference such as `risk.territory`, `coverage.limit` or a step name.
    | Ref of path: string list
    /// `#table[arg, ...]`
    | Lookup of table: string * args: Expr list
    /// `name(arg, ...)`
    | Call of name: string * args: Expr list
    | Unary of UnaryOp * Expr
    | Binary of BinaryOp * Expr * Expr
    /// `if c1 then e1 elif c2 then e2 else e3`; the branch list is never empty.
    | If of branches: (Expr * Expr) list * otherwise: Expr
    /// `x in [a, b, c]`
    | In of Expr * Expr list

    override n.Equals(o) =
        match o with
        | :? Node as other ->
            match n, other with
            | Literal a, Literal b -> a = b
            | Ref a, Ref b -> a = b
            | Lookup(t1, a1), Lookup(t2, a2) -> t1 = t2 && a1 = a2
            | Call(f1, a1), Call(f2, a2) -> f1 = f2 && a1 = a2
            | Unary(o1, e1), Unary(o2, e2) -> o1 = o2 && e1 = e2
            | Binary(o1, l1, r1), Binary(o2, l2, r2) -> o1 = o2 && l1 = l2 && r1 = r2
            | If(b1, e1), If(b2, e2) -> b1 = b2 && e1 = e2
            | In(x1, s1), In(x2, s2) -> x1 = x2 && s1 = s2
            | _ -> false
        | _ -> false

    override n.GetHashCode() =
        match n with
        | Literal v -> hash (0, v)
        | Ref p -> hash (1, p)
        | Lookup(t, a) -> hash (2, t, a)
        | Call(f, a) -> hash (3, f, a)
        | Unary(o, e) -> hash (4, o, e)
        | Binary(o, l, r) -> hash (5, o, l, r)
        | If(b, e) -> hash (6, b, e)
        | In(x, s) -> hash (7, x, s)

/// `name := expr`. Each step becomes one line of the rating worksheet.
type Step = { Name: string; Expr: Expr; Pos: Pos }

/// A parsed rating algorithm: an ordered list of steps, one of which must be `premium`.
type Algorithm = { Steps: Step list; Source: string }

[<RequireQualifiedAccess>]
module Expr =

    let at pos node = { Node = node; Pos = pos }

    let mk node = { Node = node; Pos = Pos.Start }

    let num (d: decimal) = mk (Literal(VNumber d))
    let text s = mk (Literal(VText s))
    let bool b = mk (Literal(VBool b))
    let ref (path: string) = mk (Ref(List.ofArray (path.Split '.')))

    /// Every reference path mentioned anywhere in the expression, in order of first appearance.
    let rec references (e: Expr) : string list list =
        match e.Node with
        | Literal _ -> []
        | Ref p -> [ p ]
        | Lookup(_, args)
        | Call(_, args) -> args |> List.collect references
        | Unary(_, x) -> references x
        | Binary(_, l, r) -> references l @ references r
        | If(branches, otherwise) ->
            (branches |> List.collect (fun (c, v) -> references c @ references v))
            @ references otherwise
        | In(x, set) -> references x @ (set |> List.collect references)

    let rec tables (e: Expr) : string list =
        match e.Node with
        | Literal _
        | Ref _ -> []
        | Lookup(t, args) -> t :: (args |> List.collect tables)
        | Call(_, args) -> args |> List.collect tables
        | Unary(_, x) -> tables x
        | Binary(_, l, r) -> tables l @ tables r
        | If(branches, otherwise) ->
            (branches |> List.collect (fun (c, v) -> tables c @ tables v))
            @ tables otherwise
        | In(x, set) -> tables x @ (set |> List.collect tables)

    /// Number of nodes; used to bound generated trees in property tests and to report complexity.
    let rec size (e: Expr) =
        match e.Node with
        | Literal _
        | Ref _ -> 1
        | Lookup(_, args)
        | Call(_, args) -> 1 + List.sumBy size args
        | Unary(_, x) -> 1 + size x
        | Binary(_, l, r) -> 1 + size l + size r
        | If(branches, otherwise) -> 1 + List.sumBy (fun (c, v) -> size c + size v) branches + size otherwise
        | In(x, set) -> 1 + size x + List.sumBy size set
