namespace Rater.Dsl

open Rater

type Severity =
    | Error
    | Warning

type Diagnostic =
    { Severity: Severity
      Message: string
      Pos: Pos }

    override d.ToString() =
        let tag =
            match d.Severity with
            | Error -> "error"
            | Warning -> "warning"

        $"{d.Pos}: {tag}: {d.Message}"

/// Everything an expression may refer to, other than its own algorithm's steps.
type TypeEnv =
    { Attributes: Map<string, DataType>
      /// `coverage.*` is only meaningful while rating a coverage, not in underwriting rules.
      AllowCoverage: bool
      /// Coverage codes whose final premium may be referenced as `premium.CODE`.
      PremiumRefs: Set<string>
      Tables: Map<string, Dimension list> }

type CheckedAlgorithm =
    { StepTypes: Map<string, DataType>
      Warnings: Diagnostic list }

[<RequireQualifiedAccess>]
module TypeCheck =

    /// Fields available under `coverage.` and `policy.`.
    let coverageFields = Map.ofList [ "limit", TNumber; "deductible", TNumber; "code", TText ]

    let policyFields =
        Map.ofList
            [ "effective_date", TDate
              "expiration_date", TDate
              "term_days", TNumber
              "renewal", TBool ]

    let reservedNames = set [ "risk"; "coverage"; "policy"; "premium" ]

    type private Checker(env: TypeEnv, diagnostics: ResizeArray<Diagnostic>) =

        member _.Fail(pos, msg) =
            diagnostics.Add
                { Severity = Error
                  Message = msg
                  Pos = pos }

            None

        member _.Warn(pos, msg) =
            diagnostics.Add
                { Severity = Warning
                  Message = msg
                  Pos = pos }

        member private this.Unknown(pos, kind: string, name: string, candidates: string seq) =
            let hint =
                match String.suggest candidates name with
                | Some s -> $"; did you mean '{s}'?"
                | None -> ""

            this.Fail(pos, $"unknown {kind} '{name}'{hint}")

        member this.ResolveRef(pos: Pos, path: string list, steps: Map<string, DataType>) : DataType option =
            match path with
            | [ "risk"; attr ] ->
                match env.Attributes.TryFind attr with
                | Some t -> Some t
                | None -> this.Unknown(pos, "risk attribute", attr, env.Attributes.Keys)
            | [ "coverage"; field ] when env.AllowCoverage ->
                match coverageFields.TryFind field with
                | Some t -> Some t
                | None -> this.Unknown(pos, "coverage field", field, coverageFields.Keys)
            | "coverage" :: _ when not env.AllowCoverage ->
                this.Fail(pos, "'coverage' is only available inside a coverage rating algorithm")
            | [ "policy"; field ] ->
                match policyFields.TryFind field with
                | Some t -> Some t
                | None -> this.Unknown(pos, "policy field", field, policyFields.Keys)
            | [ "premium"; code ] ->
                if env.PremiumRefs.Contains code then
                    Some TNumber
                else
                    this.Unknown(pos, "coverage premium", code, env.PremiumRefs)
            | [ name ] when reservedNames.Contains name ->
                this.Fail(pos, $"'{name}' is a namespace; write {name}.<field>")
            | [ name ] ->
                match steps.TryFind name with
                | Some t -> Some t
                | None -> this.Unknown(pos, "name", name, steps.Keys)
            | _ ->
                let dotted = String.joinWith "." path
                this.Fail(pos, $"'{dotted}' is not a valid reference")

        member this.Check(e: Expr, steps: Map<string, DataType>) : DataType option =
            let check x = this.Check(x, steps)

            match e.Node with
            | Literal v -> Some v.Type
            | Ref path -> this.ResolveRef(e.Pos, path, steps)
            | Lookup(table, args) ->
                let argTypes = args |> List.map check

                match env.Tables.TryFind table with
                | None -> this.Unknown(e.Pos, "table", table, env.Tables.Keys)
                | Some dims when dims.Length <> args.Length ->
                    let names = dims |> List.map (fun d -> d.Name) |> String.joinWith ", "
                    this.Fail(e.Pos, $"table '{table}' takes {dims.Length} argument(s) ({names}) but was given {args.Length}")
                | Some dims ->
                    let mismatches =
                        List.zip3 dims args argTypes
                        |> List.choose (fun (d, a, t) ->
                            match t with
                            | Some t when t <> d.ArgumentType ->
                                Some(a.Pos, $"table '{table}' dimension '{d.Name}' expects a {d.ArgumentType} but was given a {t}")
                            | _ -> None)

                    mismatches |> List.iter (fun (p, m) -> this.Fail(p, m) |> ignore)

                    if mismatches.IsEmpty && List.forall Option.isSome argTypes then
                        Some TNumber
                    else
                        None
            | Call(name, args) ->
                let argTypes = args |> List.map check

                match Builtins.tryFind name with
                | None -> this.Unknown(e.Pos, "function", name, Builtins.names)
                | Some b ->
                    if List.forall Option.isSome argTypes then
                        match Builtins.resolve b (List.choose id argTypes) with
                        | Ok t -> Some t
                        | Result.Error msg -> this.Fail(e.Pos, msg)
                    else
                        None
            | Unary(Negate, x) ->
                match check x with
                | Some TNumber -> Some TNumber
                | Some t -> this.Fail(e.Pos, $"cannot negate a {t}")
                | None -> None
            | Unary(Not, x) ->
                match check x with
                | Some TBool -> Some TBool
                | Some t -> this.Fail(e.Pos, $"'not' expects a bool but was given a {t}")
                | None -> None
            | Binary(op, l, r) ->
                match check l, check r with
                | Some lt, Some rt -> this.CheckBinary(e, op, lt, rt, r)
                | _ -> None
            | If(branches, otherwise) ->
                let branchTypes =
                    branches
                    |> List.map (fun (c, v) ->
                        match check c with
                        | Some TBool
                        | None -> ()
                        | Some t -> this.Fail(c.Pos, $"an 'if' condition must be a bool but is a {t}") |> ignore

                        v, check v)

                let all = branchTypes @ [ otherwise, check otherwise ]

                let known =
                    all |> List.choose (fun (x, t) -> t |> Option.map (fun t -> x, t))

                match known with
                | [] -> None
                | (_, first) :: rest ->
                    match rest |> List.tryFind (fun (_, t) -> t <> first) with
                    | Some(x, t) ->
                        this.Fail(x.Pos, $"'if' branches must agree: this branch is a {t} but an earlier one is a {first}")
                    | None when known.Length = all.Length -> Some first
                    | None -> None
            | In(x, items) ->
                let xt = check x
                let itemTypes = items |> List.map (fun i -> i, check i)

                if items.IsEmpty then
                    this.Warn(e.Pos, "'in []' is always false")

                match xt with
                | None -> None
                | Some xt ->
                    let bad =
                        itemTypes
                        |> List.choose (fun (i, t) ->
                            match t with
                            | Some t when t <> xt -> Some(i, t)
                            | _ -> None)

                    match bad with
                    | [] when itemTypes |> List.forall (snd >> Option.isSome) -> Some TBool
                    | [] -> None
                    | (i, t) :: _ -> this.Fail(i.Pos, $"'in' list holds a {t} but the value tested is a {xt}")

        member this.CheckBinary(e: Expr, op: BinaryOp, lt: DataType, rt: DataType, right: Expr) =
            let opName = Printer.operator op

            match op with
            | Add
            | Sub
            | Mul
            | Div ->
                if lt = TNumber && rt = TNumber then
                    match op, right.Node with
                    | Div, Literal(VNumber 0m) -> this.Fail(right.Pos, "division by zero")
                    | _ -> Some TNumber
                else
                    this.Fail(e.Pos, $"'{opName}' needs two numbers but was given a {lt} and a {rt}")
            | And
            | Or ->
                if lt = TBool && rt = TBool then
                    Some TBool
                else
                    this.Fail(e.Pos, $"'{opName}' needs two bools but was given a {lt} and a {rt}")
            | Eq
            | Ne ->
                if lt = rt then
                    Some TBool
                else
                    this.Fail(e.Pos, $"cannot compare a {lt} with a {rt}")
            | Lt
            | Le
            | Gt
            | Ge ->
                if lt <> rt then
                    this.Fail(e.Pos, $"cannot compare a {lt} with a {rt}")
                elif lt = TNumber || lt = TDate then
                    Some TBool
                else
                    this.Fail(e.Pos, $"'{opName}' orders numbers and dates, not {lt} values")

    /// Type-checks a standalone expression (for example an underwriting condition).
    let expression (env: TypeEnv) (e: Expr) : Result<DataType * Diagnostic list, Diagnostic list> =
        let diags = ResizeArray()
        let t = Checker(env, diags).Check(e, Map.empty)
        let errors, warnings = diags |> List.ofSeq |> List.partition (fun d -> d.Severity = Error)

        match t with
        | Some t when errors.IsEmpty -> Ok(t, warnings)
        | _ -> Result.Error(errors @ warnings |> List.filter (fun d -> d.Severity = Error))

    /// Checks a whole algorithm: every step in order, then the rules about steps themselves.
    let algorithm (env: TypeEnv) (a: Algorithm) : Result<CheckedAlgorithm, Diagnostic list> =
        let diags = ResizeArray()
        let checker = Checker(env, diags)

        let steps =
            a.Steps
            |> List.fold
                (fun (types: Map<string, DataType>) step ->
                    if reservedNames.Contains step.Name then
                        checker.Fail(step.Pos, $"'{step.Name}' is reserved and cannot be used as a step name")
                        |> ignore

                        types
                    elif types.ContainsKey step.Name then
                        checker.Fail(step.Pos, $"step '{step.Name}' is defined more than once") |> ignore
                        types
                    elif Builtins.tryFind step.Name |> Option.isSome then
                        checker.Fail(step.Pos, $"'{step.Name}' is a built-in function and cannot be used as a step name")
                        |> ignore

                        types
                    else
                        match checker.Check(step.Expr, types) with
                        | Some t -> types.Add(step.Name, t)
                        | None -> types)
                Map.empty

        match a.Steps |> List.tryFindIndex (fun s -> s.Name = "premium") with
        | None ->
            checker.Fail(Pos.Start, "the algorithm must define a 'premium' step") |> ignore
        | Some i ->
            let premiumStep = a.Steps[i]

            match steps.TryFind "premium" with
            | Some t when t <> TNumber ->
                checker.Fail(premiumStep.Pos, $"'premium' must be a number but is a {t}") |> ignore
            | _ -> ()

            a.Steps
            |> List.skip (i + 1)
            |> List.iter (fun s -> checker.Warn(s.Pos, $"step '{s.Name}' comes after 'premium' and has no effect"))

        let used =
            a.Steps
            |> List.collect (fun s -> Expr.references s.Expr)
            |> List.choose (function
                | [ name ] -> Some name
                | _ -> None)
            |> Set.ofList

        a.Steps
        |> List.filter (fun s -> s.Name <> "premium" && not (used.Contains s.Name))
        |> List.iter (fun s -> checker.Warn(s.Pos, $"step '{s.Name}' is never used"))

        let all = List.ofSeq diags
        let errors = all |> List.filter (fun d -> d.Severity = Error)

        if errors.IsEmpty then
            Ok
                { StepTypes = steps
                  Warnings = all |> List.filter (fun d -> d.Severity = Warning) }
        else
            Result.Error errors
