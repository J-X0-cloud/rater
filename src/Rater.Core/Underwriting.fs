namespace Rater

open Rater.Dsl

/// A rule that fired, or that could not be evaluated.
type Finding =
    { RuleId: string
      Action: UnderwritingAction
      Message: string }

/// The underwriting outcome for a quote. Any decline wins over any referral.
type Decision =
    | Accept
    | Referred of Finding list
    | Declined of Finding list

    member d.Name =
        match d with
        | Accept -> "accept"
        | Referred _ -> "refer"
        | Declined _ -> "decline"

[<RequireQualifiedAccess>]
module Underwriting =

    /// Evaluates every rule. A rule whose condition fails to evaluate (for example because a
    /// table row is missing) is treated as a referral: an engine that cannot tell whether a risk
    /// is acceptable must not quietly accept it.
    let evaluate (plan: RatingPlan) (ctx: EvalContext) : Finding list =
        plan.Product.Rules
        |> List.choose (fun rule ->
            match plan.RuleCode.TryFind rule.Id with
            | None -> None
            | Some code ->
                match code ctx with
                | Ok(VBool true) ->
                    Some
                        { RuleId = rule.Id
                          Action = rule.Action
                          Message = rule.Message }
                | Ok _ -> None
                | Error e ->
                    Some
                        { RuleId = rule.Id
                          Action = Refer
                          Message = $"rule could not be evaluated ({e.Message}); refer for manual review" })

    let decide (findings: Finding list) =
        let declines = findings |> List.filter (fun f -> f.Action = Decline)
        let referrals = findings |> List.filter (fun f -> f.Action = Refer)

        if not declines.IsEmpty then Declined declines
        elif not referrals.IsEmpty then Referred referrals
        else Accept
