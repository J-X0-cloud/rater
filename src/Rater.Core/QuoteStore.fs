namespace Rater

open System

/// The quote and the events that produced it, as stored.
type QuoteRecord =
    { Quote: Quote
      Events: QuoteEvent list }

type private StoreMessage =
    | Execute of
        id: string *
        command: QuoteCommand *
        expectedVersion: int option *
        reply: AsyncReplyChannel<Result<QuoteRecord, LifecycleError>>
    | Get of id: string * reply: AsyncReplyChannel<QuoteRecord option>
    | List of reply: AsyncReplyChannel<Quote list>
    | ExpireStale of reply: AsyncReplyChannel<string list>

/// An in-memory, event-sourced quote store. A single mailbox serialises every command, so two
/// concurrent binds of the same quote cannot both succeed; optimistic version checks protect
/// clients that read a quote and act on it later.
type QuoteStore(deps: LifecycleDeps) =

    let apply (records: Map<string, QuoteRecord>) id command expected =
        let current = records.TryFind id

        let versionCheck =
            match expected, current with
            | Some v, Some r when r.Quote.Version <> v -> Error(VersionConflict(v, r.Quote.Version))
            | Some _, None -> Error(QuoteNotFound id)
            | _ -> Ok()

        versionCheck
        |> Result.bind (fun () ->
            match current, command with
            | None, Create _ -> Ok()
            | None, _ -> Error(QuoteNotFound id)
            | Some _, _ -> Ok())
        |> Result.bind (fun () -> Quote.decide deps (current |> Option.map (fun r -> r.Quote)) command)
        |> Result.map (fun events ->
            let previous = current |> Option.map (fun r -> r.Events) |> Option.defaultValue []

            let state =
                events
                |> List.fold (fun s e -> Some(Quote.evolve s e)) (current |> Option.map (fun r -> r.Quote))

            { Quote = state.Value
              Events = previous @ events })

    let agent =
        MailboxProcessor.Start(fun inbox ->
            let rec loop (records: Map<string, QuoteRecord>) =
                async {
                    let! msg = inbox.Receive()

                    match msg with
                    | Execute(id, command, expected, reply) ->
                        let outcome =
                            try
                                apply records id command expected
                            with ex ->
                                Error(InvalidTransition(ex.Message, "unavailable"))

                        reply.Reply outcome

                        match outcome with
                        | Ok record -> return! loop (records.Add(record.Quote.Id, record))
                        | Error _ -> return! loop records
                    | Get(id, reply) ->
                        reply.Reply(records.TryFind id)
                        return! loop records
                    | List reply ->
                        reply.Reply(
                            records
                            |> Map.toList
                            |> List.map (fun (_, r) -> r.Quote)
                            |> List.sortBy (fun q -> q.CreatedAt, q.Id)
                        )

                        return! loop records
                    | ExpireStale reply ->
                        let mutable updated = records
                        let expired = ResizeArray()

                        for KeyValue(id, record) in records do
                            match record.Quote.Status, record.Quote.ValidUntil with
                            | (Quoted | Referred), Some _ ->
                                match apply updated id Expire None with
                                | Ok r ->
                                    updated <- updated.Add(id, r)
                                    expired.Add id
                                | Error _ -> ()
                            | _ -> ()

                        reply.Reply(List.ofSeq expired)
                        return! loop updated
                }

            loop Map.empty)

    /// Runs a command. Creating a quote takes its id from the command.
    member _.Execute(command: QuoteCommand, ?id: string, ?expectedVersion: int) =
        let id =
            match command, id with
            | Create(newId, _, _), _ -> newId
            | _, Some id -> id
            | _, None -> invalidArg "id" "an id is required for every command except Create"

        agent.PostAndAsyncReply(fun reply -> Execute(id, command, expectedVersion, reply))

    member _.Get(id: string) =
        agent.PostAndAsyncReply(fun reply -> Get(id, reply))

    member _.List() = agent.PostAndAsyncReply List

    /// Expires every quoted or referred quote whose validity has lapsed; returns their ids.
    member _.ExpireStale() = agent.PostAndAsyncReply ExpireStale
