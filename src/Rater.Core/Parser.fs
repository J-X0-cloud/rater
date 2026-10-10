namespace Rater.Dsl

open Rater

/// A Pratt parser for the rating language.
///
/// Precedence, loosest first: `or`, `and`, `not`, comparisons and `in` (non-associative),
/// `+ -`, `* /`, unary minus. Statements are `name := expr` and need no separator: an expression
/// ends as soon as the next token cannot continue it. A `;` may be used for clarity.
[<RequireQualifiedAccess>]
module Parser =

    type private State(tokens: Located array) =
        let mutable index = 0

        member _.Peek = tokens[index]

        member _.PeekAt k =
            tokens[min (index + k) (tokens.Length - 1)]

        member _.Next() =
            let t = tokens[index]

            if index < tokens.Length - 1 then
                index <- index + 1

            t

    let private fail pos msg =
        raise (SyntaxException { Message = msg; Pos = pos })

    let private expect (s: State) (tok: Token) (context: string) =
        let t = s.Next()

        if t.Token <> tok then
            fail t.Pos $"expected {tok} {context} but found {t.Token}"

        t

    let private bindingPower =
        function
        | TokKeyword "or" -> 10
        | TokKeyword "and" -> 20
        | TokOp("==" | "!=" | "<" | "<=" | ">" | ">=")
        | TokKeyword "in" -> 40
        | TokOp("+" | "-") -> 50
        | TokOp("*" | "/") -> 60
        | _ -> 0

    let private binaryOp =
        function
        | TokKeyword "or" -> Or
        | TokKeyword "and" -> And
        | TokOp "==" -> Eq
        | TokOp "!=" -> Ne
        | TokOp "<" -> Lt
        | TokOp "<=" -> Le
        | TokOp ">" -> Gt
        | TokOp ">=" -> Ge
        | TokOp "+" -> Add
        | TokOp "-" -> Sub
        | TokOp "*" -> Mul
        | TokOp "/" -> Div
        | t -> invalidOp $"{t} is not a binary operator"

    let private notPower = 30
    let private negatePower = 70

    let rec private parseExpr (s: State) (minPower: int) : Expr =
        let mutable left = parsePrefix s
        let mutable go = true

        while go do
            let t = s.Peek
            let power = bindingPower t.Token

            if power <= minPower then
                go <- false
            elif t.Token = TokKeyword "in" then
                s.Next() |> ignore
                let items = parseList s TokLBracket TokRBracket "after 'in'"
                left <- Expr.at t.Pos (In(left, items))
                rejectChainedComparison s
            else
                s.Next() |> ignore
                let isComparison = power = 40
                let right = parseExpr s power
                left <- Expr.at t.Pos (Binary(binaryOp t.Token, left, right))

                if isComparison then
                    rejectChainedComparison s

        left

    and private rejectChainedComparison (s: State) =
        if bindingPower s.Peek.Token = 40 then
            fail s.Peek.Pos "comparisons cannot be chained; combine them with 'and'"

    and private parseList (s: State) (openTok: Token) (closeTok: Token) (context: string) : Expr list =
        expect s openTok context |> ignore

        if s.Peek.Token = closeTok then
            s.Next() |> ignore
            []
        else
            let items = ResizeArray [ parseExpr s 0 ]

            while s.Peek.Token = TokComma do
                s.Next() |> ignore
                items.Add(parseExpr s 0)

            expect s closeTok context |> ignore
            List.ofSeq items

    and private parsePrefix (s: State) : Expr =
        let t = s.Next()

        match t.Token with
        | TokNumber n -> Expr.at t.Pos (Literal(VNumber n))
        | TokString str -> Expr.at t.Pos (Literal(VText str))
        | TokDate d -> Expr.at t.Pos (Literal(VDate d))
        | TokKeyword "true" -> Expr.at t.Pos (Literal(VBool true))
        | TokKeyword "false" -> Expr.at t.Pos (Literal(VBool false))
        | TokOp "-" ->
            match s.Peek.Token with
            | TokNumber n ->
                // A minus written directly against a number is part of the literal.
                s.Next() |> ignore
                Expr.at t.Pos (Literal(VNumber(-n)))
            | _ -> Expr.at t.Pos (Unary(Negate, parseExpr s negatePower))
        | TokKeyword "not" -> Expr.at t.Pos (Unary(Not, parseExpr s notPower))
        | TokLParen ->
            let inner = parseExpr s 0
            expect s TokRParen "to close '('" |> ignore
            inner
        | TokTable name ->
            let args = parseList s TokLBracket TokRBracket $"after table '#{name}'"
            Expr.at t.Pos (Lookup(name, args))
        | TokKeyword "if" -> parseIf s t.Pos
        | TokIdent name when s.Peek.Token = TokLParen ->
            let args = parseList s TokLParen TokRParen $"in call to '{name}'"
            Expr.at t.Pos (Call(name, args))
        | TokIdent name ->
            let path = ResizeArray [ name ]

            while s.Peek.Token = TokDot do
                s.Next() |> ignore

                match s.Next() with
                | { Token = TokIdent part } -> path.Add part
                | other -> fail other.Pos $"expected a name after '.' but found {other.Token}"

            Expr.at t.Pos (Ref(List.ofSeq path))
        | other -> fail t.Pos $"expected an expression but found {other}"

    and private parseIf (s: State) (pos: Pos) : Expr =
        let branches = ResizeArray()
        let cond = parseExpr s 0
        expect s (TokKeyword "then") "after the 'if' condition" |> ignore
        branches.Add(cond, parseExpr s 0)

        while s.Peek.Token = TokKeyword "elif" do
            s.Next() |> ignore
            let c = parseExpr s 0
            expect s (TokKeyword "then") "after the 'elif' condition" |> ignore
            branches.Add(c, parseExpr s 0)

        expect s (TokKeyword "else") "to complete the 'if' expression (every branch needs a value)"
        |> ignore

        let otherwise = parseExpr s 0
        Expr.at pos (If(List.ofSeq branches, otherwise))

    let private run (source: string) (body: State -> 'a) : Result<'a, SyntaxError> =
        match Lexer.tokenize source with
        | Error e -> Error e
        | Ok tokens ->
            let s = State(Array.ofList tokens)

            try
                let v = body s

                match s.Peek with
                | { Token = TokEnd } -> Ok v
                | t ->
                    Error
                        { Message = $"unexpected {t.Token}"
                          Pos = t.Pos }
            with SyntaxException e ->
                Error e

    /// Parses a single expression, e.g. an underwriting rule condition.
    let parseExpression (source: string) : Result<Expr, SyntaxError> = run source (fun s -> parseExpr s 0)

    /// Parses an algorithm: a sequence of `name := expr` steps.
    let parseAlgorithm (source: string) : Result<Algorithm, SyntaxError> =
        run source (fun s ->
            let steps = ResizeArray()

            while s.Peek.Token <> TokEnd do
                match s.Next() with
                | { Token = TokIdent name; Pos = pos } ->
                    if s.Peek.Token = TokDot then
                        fail s.Peek.Pos $"step names cannot contain '.'; '{name}.' looks like a reference"

                    expect s TokAssign $"after step name '{name}'" |> ignore
                    let expr = parseExpr s 0
                    steps.Add { Name = name; Expr = expr; Pos = pos }

                    while s.Peek.Token = TokSemicolon do
                        s.Next() |> ignore
                | { Token = TokSemicolon } -> ()
                | t -> fail t.Pos $"expected a step of the form 'name := expression' but found {t.Token}"

            { Steps = List.ofSeq steps
              Source = source })
