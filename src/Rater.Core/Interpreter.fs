namespace Rater.Dsl

open System
open System.Collections.Generic
open Rater

/// The coverage being rated, as seen by `coverage.*` references.
type CoverageContext =
    { Code: string
      Limit: decimal
      Deductible: decimal }

/// Policy-level facts visible to every expression as `policy.*`.
type PolicyContext =
    { EffectiveDate: DateOnly
      ExpirationDate: DateOnly
      Renewal: bool }

    member p.TermDays = Dates.daysBetween p.EffectiveDate p.ExpirationDate

/// Everything an evaluation can read. Tables are already resolved to the version in force.
type EvalContext =
    { Risk: IReadOnlyDictionary<string, Value>
      Coverage: CoverageContext option
      Policy: PolicyContext
      Premiums: IReadOnlyDictionary<string, decimal>
      Tables: string -> Table option }

type RuntimeError =
    { Message: string
      Pos: Pos
      Step: string option }

    override e.ToString() =
        match e.Step with
        | Some s -> $"step '{s}' at {e.Pos}: {e.Message}"
        | None -> $"{e.Pos}: {e.Message}"

exception internal EvalException of message: string * pos: Pos

/// One line of a rating worksheet: the step, its formula as filed, the value it produced, and
/// every table lookup it made along the way.
type WorksheetLine =
    { Step: string
      Formula: string
      Value: Value
      Lookups: LookupTrace list }

type Worksheet =
    { Lines: WorksheetLine list
      Premium: decimal }

/// Per-evaluation mutable state, kept out of the immutable context.
type internal Frame(ctx: EvalContext, slots: Value array) =
    let lookups = ResizeArray<LookupTrace>()
    member _.Ctx = ctx
    member _.Slots = slots
    member _.Lookups = lookups

/// Compiles expressions into closures once, so each quote only pays for evaluation.
///
/// Compilation resolves step names to array slots and validates nothing that the type checker
/// already guarantees; evaluation errors (a missing table row, division by zero, overflow) are
/// reported with the source position of the node that failed.
[<RequireQualifiedAccess>]
module Interpreter =

    type internal Compiled = Frame -> Value

    let private fail pos msg = raise (EvalException(msg, pos))

    let private asNumber pos (v: Value) =
        match v with
        | VNumber n -> n
        | other -> fail pos $"expected a number but found {Value.describe other}"

    let private asBool pos (v: Value) =
        match v with
        | VBool b -> b
        | other -> fail pos $"expected a bool but found {Value.describe other}"

    let private arithmetic pos (f: decimal -> decimal -> decimal) (l: Compiled) (r: Compiled) : Compiled =
        fun frame ->
            let a = asNumber pos (l frame)
            let b = asNumber pos (r frame)

            try
                VNumber(f a b)
            with
            | :? OverflowException -> fail pos "arithmetic overflow"
            | :? DivideByZeroException -> fail pos "division by zero"

    let private ordering pos (test: int -> bool) (l: Compiled) (r: Compiled) : Compiled =
        fun frame ->
            let a = l frame
            let b = r frame

            if a.Type <> b.Type then
                fail pos $"cannot compare {Value.describe a} with {Value.describe b}"

            VBool(test (compare a b))

    let rec internal compile (slotOf: string -> int option) (e: Expr) : Compiled =
        let pos = e.Pos
        let recurse = compile slotOf

        match e.Node with
        | Literal v -> fun _ -> v
        | Ref [ "risk"; attr ] ->
            fun frame ->
                match frame.Ctx.Risk.TryGetValue attr with
                | true, v -> v
                | _ -> fail pos $"risk attribute '{attr}' was not supplied"
        | Ref [ "coverage"; field ] ->
            fun frame ->
                match frame.Ctx.Coverage with
                | None -> fail pos "no coverage is being rated"
                | Some c ->
                    match field with
                    | "limit" -> VNumber c.Limit
                    | "deductible" -> VNumber c.Deductible
                    | "code" -> VText c.Code
                    | other -> fail pos $"unknown coverage field '{other}'"
        | Ref [ "policy"; field ] ->
            match field with
            | "effective_date" -> fun frame -> VDate frame.Ctx.Policy.EffectiveDate
            | "expiration_date" -> fun frame -> VDate frame.Ctx.Policy.ExpirationDate
            | "term_days" -> fun frame -> VNumber(decimal frame.Ctx.Policy.TermDays)
            | "renewal" -> fun frame -> VBool frame.Ctx.Policy.Renewal
            | other -> fun _ -> fail pos $"unknown policy field '{other}'"
        | Ref [ "premium"; code ] ->
            fun frame ->
                match frame.Ctx.Premiums.TryGetValue code with
                | true, p -> VNumber p
                | _ -> fail pos $"coverage '{code}' has not been rated on this quote"
        | Ref [ name ] ->
            match slotOf name with
            | Some slot -> fun frame -> frame.Slots[slot]
            | None -> fun _ -> fail pos $"unknown name '{name}'"
        | Ref path ->
            let dotted = String.joinWith "." path
            fun _ -> fail pos $"'{dotted}' is not a valid reference"
        | Lookup(tableName, args) ->
            let compiledArgs = args |> List.map recurse

            fun frame ->
                match frame.Ctx.Tables tableName with
                | None -> fail pos $"table '{tableName}' is not in force for this policy"
                | Some table ->
                    let values = compiledArgs |> List.map (fun a -> a frame)

                    match Table.lookup table values with
                    | Ok trace ->
                        frame.Lookups.Add trace
                        VNumber trace.Result
                    | Error err -> fail pos (string err)
        | Call(name, args) ->
            let compiledArgs = args |> List.map recurse

            match Builtins.tryFind name with
            | None -> fun _ -> fail pos $"unknown function '{name}'"
            | Some b ->
                fun frame ->
                    let values = compiledArgs |> List.map (fun a -> a frame)

                    try
                        b.Invoke values
                    with
                    | BuiltinError msg -> fail pos msg
                    | :? OverflowException -> fail pos $"{name}: arithmetic overflow"
                    | :? DivideByZeroException -> fail pos $"{name}: division by zero"
        | Unary(Negate, x) ->
            let cx = recurse x
            fun frame -> VNumber(-(asNumber pos (cx frame)))
        | Unary(Not, x) ->
            let cx = recurse x
            fun frame -> VBool(not (asBool pos (cx frame)))
        | Binary(op, l, r) ->
            let cl = recurse l
            let cr = recurse r

            match op with
            | Add -> arithmetic pos (+) cl cr
            | Sub -> arithmetic pos (-) cl cr
            | Mul -> arithmetic pos (*) cl cr
            | Div ->
                arithmetic
                    pos
                    (fun a b ->
                        if b = 0m then
                            fail pos $"division by zero ({Decimal.toInvariantString a} / 0)"

                        a / b)
                    cl
                    cr
            | And -> fun frame -> VBool(asBool pos (cl frame) && asBool pos (cr frame))
            | Or -> fun frame -> VBool(asBool pos (cl frame) || asBool pos (cr frame))
            | Eq -> fun frame -> VBool((cl frame) = (cr frame))
            | Ne -> fun frame -> VBool((cl frame) <> (cr frame))
            | Lt -> ordering pos (fun c -> c < 0) cl cr
            | Le -> ordering pos (fun c -> c <= 0) cl cr
            | Gt -> ordering pos (fun c -> c > 0) cl cr
            | Ge -> ordering pos (fun c -> c >= 0) cl cr
        | If(branches, otherwise) ->
            let compiledBranches =
                branches |> List.map (fun (c, v) -> c.Pos, recurse c, recurse v) |> Array.ofList

            let co = recurse otherwise

            fun frame ->
                let mutable result = None
                let mutable i = 0

                while result.IsNone && i < compiledBranches.Length do
                    let cpos, cond, value = compiledBranches[i]

                    if asBool cpos (cond frame) then
                        result <- Some(value frame)

                    i <- i + 1

                match result with
                | Some v -> v
                | None -> co frame
        | In(x, items) ->
            let cx = recurse x
            let citems = items |> List.map recurse

            fun frame ->
                let v = cx frame
                VBool(citems |> List.exists (fun c -> c frame = v))

    /// A compiled algorithm, reusable across any number of evaluations.
    type CompiledAlgorithm =
        internal
            { Steps: (Step * string * Compiled) array
              PremiumSlot: int }

        member a.StepNames = a.Steps |> Array.map (fun (s, _, _) -> s.Name) |> List.ofArray

    /// Compiles a (type-checked) algorithm. Fails only if `premium` is missing.
    let compileAlgorithm (a: Algorithm) : Result<CompiledAlgorithm, string> =
        let slots =
            a.Steps |> List.mapi (fun i s -> s.Name, i) |> List.distinctBy fst |> Map.ofList

        match slots.TryFind "premium" with
        | None -> Error "the algorithm does not define 'premium'"
        | Some premiumSlot ->
            let compiled =
                a.Steps
                |> List.mapi (fun i s ->
                    // A step may only see the steps before it.
                    let visible name =
                        slots.TryFind name |> Option.filter (fun slot -> slot < i)

                    s, Printer.expr s.Expr, compile visible s.Expr)
                |> Array.ofList

            Ok
                { Steps = compiled
                  PremiumSlot = premiumSlot }

    /// Runs a compiled algorithm, producing a full worksheet.
    let run (ctx: EvalContext) (a: CompiledAlgorithm) : Result<Worksheet, RuntimeError> =
        let slots = Array.create a.Steps.Length (VNumber 0m)
        let frame = Frame(ctx, slots)
        let lines = ResizeArray()
        let mutable failure = None
        let mutable i = 0

        while failure.IsNone && i <= a.PremiumSlot do
            let step, formula, code = a.Steps[i]
            let before = frame.Lookups.Count

            try
                let v = code frame
                slots[i] <- v

                lines.Add
                    { Step = step.Name
                      Formula = formula
                      Value = v
                      Lookups = frame.Lookups |> Seq.skip before |> List.ofSeq }
            with EvalException(msg, pos) ->
                failure <-
                    Some
                        { Message = msg
                          Pos = pos
                          Step = Some step.Name }

            i <- i + 1

        match failure with
        | Some err -> Error err
        | None ->
            match slots[a.PremiumSlot] with
            | VNumber p ->
                Ok
                    { Lines = List.ofSeq lines
                      Premium = p }
            | other ->
                Error
                    { Message = $"premium evaluated to {Value.describe other}, not a number"
                      Pos = (let s, _, _ = a.Steps[a.PremiumSlot] in s.Pos)
                      Step = Some "premium" }

    /// Compiles a standalone expression with no steps in scope.
    let compileExpression (e: Expr) : EvalContext -> Result<Value, RuntimeError> =
        let code = compile (fun _ -> None) e

        fun ctx ->
            try
                Ok(code (Frame(ctx, [||])))
            with EvalException(msg, pos) ->
                Error
                    { Message = msg
                      Pos = pos
                      Step = None }
