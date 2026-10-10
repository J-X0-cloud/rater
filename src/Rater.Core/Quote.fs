namespace Rater

open System

type DeclineSource =
    | ByRules
    | ByUnderwriter of underwriter: string

type QuoteStatus =
    | Draft
    | Quoted
    | Referred
    | Declined of DeclineSource
    | Bound
    | Expired

    member s.Name =
        match s with
        | Draft -> "draft"
        | Quoted -> "quoted"
        | Referred -> "referred"
        | Declined _ -> "declined"
        | Bound -> "bound"
        | Expired -> "expired"

    member s.IsTerminal =
        match s with
        | Bound
        | Expired
        | Declined(ByUnderwriter _) -> true
        | _ -> false

type Approval =
    { RuleId: string
      Underwriter: string
      Note: string
      At: DateTimeOffset }

type QuoteEvent =
    | Created of id: string * product: string * request: RatingRequest * at: DateTimeOffset
    | Amended of request: RatingRequest * at: DateTimeOffset
    | Rated of result: RatingResult * validUntil: DateOnly * at: DateTimeOffset
    | ReferralApproved of Approval
    | DeclinedByUnderwriter of underwriter: string * reason: string * at: DateTimeOffset
    | PolicyBound of policyNumber: string * at: DateTimeOffset
    | QuoteExpired of at: DateTimeOffset

    member e.At =
        match e with
        | Created(_, _, _, at)
        | Amended(_, at)
        | Rated(_, _, at)
        | DeclinedByUnderwriter(_, _, at)
        | PolicyBound(_, at)
        | QuoteExpired at -> at
        | ReferralApproved a -> a.At

    member e.Name =
        match e with
        | Created _ -> "created"
        | Amended _ -> "amended"
        | Rated _ -> "rated"
        | ReferralApproved _ -> "referral_approved"
        | DeclinedByUnderwriter _ -> "declined"
        | PolicyBound _ -> "bound"
        | QuoteExpired _ -> "expired"

/// The current state of a quote, derived entirely from its events.
type Quote =
    { Id: string
      Product: string
      Version: int
      Status: QuoteStatus
      Request: RatingRequest
      Rating: RatingResult option
      ValidUntil: DateOnly option
      OpenReferrals: Finding list
      Approvals: Approval list
      DeclineReason: string option
      PolicyNumber: string option
      CreatedAt: DateTimeOffset
      UpdatedAt: DateTimeOffset }

type QuoteCommand =
    | Create of id: string * product: string * request: RatingRequest
    | Amend of RatingRequest
    | Rate
    | ApproveReferral of ruleId: string * underwriter: string * note: string
    | Decline of underwriter: string * reason: string
    | Bind
    | Expire

type LifecycleError =
    | QuoteAlreadyExists of string
    | QuoteNotFound of string
    | InvalidTransition of command: string * status: string
    | RatingFailed of RatingIssue list
    | UnknownReferral of string
    | ReferralAlreadyApproved of string
    | QuoteHasExpired of DateOnly
    | EffectiveDateInPast of DateOnly
    | MissingField of string
    | VersionConflict of expected: int * actual: int

    override e.ToString() =
        match e with
        | QuoteAlreadyExists id -> $"quote '{id}' already exists"
        | QuoteNotFound id -> $"quote '{id}' does not exist"
        | InvalidTransition(c, s) -> $"cannot {c} a quote that is {s}"
        | RatingFailed issues -> $"""the quote could not be rated: {issues |> List.map string |> String.joinWith "; "}"""
        | UnknownReferral r -> $"rule '{r}' has not referred this quote"
        | ReferralAlreadyApproved r -> $"the referral from rule '{r}' is already approved"
        | QuoteHasExpired d -> $"the quote expired at the end of {Dates.iso d}"
        | EffectiveDateInPast d -> $"cannot bind a policy effective {Dates.iso d}, which is in the past"
        | MissingField f -> $"{f} is required"
        | VersionConflict(e, a) -> $"the quote changed since version {e} (it is now at version {a}); reload and retry"

/// What a lifecycle decision may consult beyond the quote itself.
type LifecycleDeps =
    { Rate: string -> RatingRequest -> Result<RatingResult, RatingIssue list>
      ValidityDays: string -> int
      Now: unit -> DateTimeOffset
      NextPolicyNumber: string -> string }

/// The quote lifecycle as a pure decide/evolve pair. `decide` validates a command against the
/// current state and emits events; `evolve` applies an event. Replaying a quote's events through
/// `evolve` always reconstructs the same state, which the property tests check.
///
///     Draft --rate--> Quoted --bind--> Bound
///       |               |  \
///       |             rate  decline
///       v               v     \
///    (accept)        Referred --approve all--> Quoted
///                       |
///                       +--decline--> Declined
///
/// Amending any non-terminal quote returns it to Draft. Quotes past their validity can only expire.
[<RequireQualifiedAccess>]
module Quote =

    let private commandName =
        function
        | Create _ -> "create"
        | Amend _ -> "amend"
        | Rate -> "rate"
        | ApproveReferral _ -> "approve a referral on"
        | Decline _ -> "decline"
        | Bind -> "bind"
        | Expire -> "expire"

    let evolve (state: Quote option) (event: QuoteEvent) : Quote =
        match state, event with
        | None, Created(id, product, request, at) ->
            { Id = id
              Product = product
              Version = 1
              Status = Draft
              Request = request
              Rating = None
              ValidUntil = None
              OpenReferrals = []
              Approvals = []
              DeclineReason = None
              PolicyNumber = None
              CreatedAt = at
              UpdatedAt = at }
        | None, e -> invalidOp $"the first event of a quote must be 'created', not '{e.Name}'"
        | Some _, Created _ -> invalidOp "a quote cannot be created twice"
        | Some q, e ->
            let q =
                { q with
                    Version = q.Version + 1
                    UpdatedAt = e.At }

            match e with
            | Created _ -> q
            | Amended(request, _) ->
                { q with
                    Status = Draft
                    Request = request
                    Rating = None
                    ValidUntil = None
                    OpenReferrals = []
                    Approvals = []
                    DeclineReason = None }
            | Rated(result, validUntil, _) ->
                let referrals, status =
                    match result.Decision with
                    | Decision.Accept -> [], Quoted
                    | Decision.Referred findings -> findings, Referred
                    | Decision.Declined _ -> [], Declined ByRules

                let reason =
                    match result.Decision with
                    | Decision.Declined findings -> Some(findings |> List.map (fun f -> f.Message) |> String.joinWith "; ")
                    | _ -> None

                { q with
                    Status = status
                    Rating = Some result
                    ValidUntil = Some validUntil
                    OpenReferrals = referrals
                    Approvals = []
                    DeclineReason = reason }
            | ReferralApproved approval ->
                let remaining =
                    q.OpenReferrals |> List.filter (fun f -> f.RuleId <> approval.RuleId)

                { q with
                    OpenReferrals = remaining
                    Approvals = q.Approvals @ [ approval ]
                    Status = if remaining.IsEmpty then Quoted else Referred }
            | DeclinedByUnderwriter(underwriter, reason, _) ->
                { q with
                    Status = Declined(ByUnderwriter underwriter)
                    DeclineReason = Some reason }
            | PolicyBound(number, _) ->
                { q with
                    Status = Bound
                    PolicyNumber = Some number }
            | QuoteExpired _ -> { q with Status = Expired }

    let replay (events: QuoteEvent seq) : Quote option =
        events |> Seq.fold (fun s e -> Some(evolve s e)) None

    let private isPastValidity (now: DateTimeOffset) (q: Quote) =
        match q.ValidUntil with
        | Some until -> DateOnly.FromDateTime now.UtcDateTime > until
        | None -> false

    let decide (deps: LifecycleDeps) (state: Quote option) (command: QuoteCommand) : Result<QuoteEvent list, LifecycleError> =
        let now = deps.Now()
        let today = DateOnly.FromDateTime now.UtcDateTime

        let invalid (q: Quote) =
            Error(InvalidTransition(commandName command, q.Status.Name))

        match state, command with
        | Some q, Create _ -> Error(QuoteAlreadyExists q.Id)
        | None, Create(id, product, request) ->
            if String.isBlank id then Error(MissingField "quote id")
            elif String.isBlank product then Error(MissingField "product")
            else Ok [ Created(id, product, request, now) ]
        | None, _ -> Error(QuoteNotFound "")
        | Some q, _ when q.Status.IsTerminal -> invalid q
        | Some q, Expire ->
            if isPastValidity now q || q.Status = Draft then
                Ok [ QuoteExpired now ]
            else
                invalid q
        | Some q, _ when isPastValidity now q -> Error(QuoteHasExpired q.ValidUntil.Value)
        | Some _, Amend request -> Ok [ Amended(request, now) ]
        | Some q, Rate ->
            match q.Status with
            | Draft ->
                match deps.Rate q.Product q.Request with
                | Ok result ->
                    let validUntil = today.AddDays(deps.ValidityDays q.Product)
                    Ok [ Rated(result, validUntil, now) ]
                | Error issues -> Error(RatingFailed issues)
            | _ -> invalid q
        | Some q, ApproveReferral(ruleId, underwriter, note) ->
            match q.Status with
            | Referred ->
                if String.isBlank underwriter then
                    Error(MissingField "underwriter")
                elif q.Approvals |> List.exists (fun a -> a.RuleId = ruleId) then
                    Error(ReferralAlreadyApproved ruleId)
                elif q.OpenReferrals |> List.exists (fun f -> f.RuleId = ruleId) |> not then
                    Error(UnknownReferral ruleId)
                else
                    Ok
                        [ ReferralApproved
                              { RuleId = ruleId
                                Underwriter = underwriter
                                Note = note
                                At = now } ]
            | _ -> invalid q
        | Some q, Decline(underwriter, reason) ->
            match q.Status with
            | Referred
            | Quoted ->
                if String.isBlank underwriter then Error(MissingField "underwriter")
                elif String.isBlank reason then Error(MissingField "decline reason")
                else Ok [ DeclinedByUnderwriter(underwriter, reason, now) ]
            | _ -> invalid q
        | Some q, Bind ->
            match q.Status with
            | Quoted when q.Request.EffectiveDate < today -> Error(EffectiveDateInPast q.Request.EffectiveDate)
            | Quoted -> Ok [ PolicyBound(deps.NextPolicyNumber q.Product, now) ]
            | _ -> invalid q
