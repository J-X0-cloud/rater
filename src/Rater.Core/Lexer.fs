namespace Rater.Dsl

open System
open System.Globalization
open System.Text
open Rater

type Token =
    | TokNumber of decimal
    | TokString of string
    | TokDate of DateOnly
    | TokIdent of string
    | TokTable of string
    | TokKeyword of string
    | TokAssign
    | TokLParen
    | TokRParen
    | TokLBracket
    | TokRBracket
    | TokComma
    | TokDot
    | TokSemicolon
    | TokOp of string
    | TokEnd

    override t.ToString() =
        match t with
        | TokNumber n -> $"number {Decimal.toInvariantString n}"
        | TokString s -> $"string \"{s}\""
        | TokDate d -> "date @" + d.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture)
        | TokIdent s -> $"identifier '{s}'"
        | TokTable s -> $"table reference '#{s}'"
        | TokKeyword k -> $"keyword '{k}'"
        | TokAssign -> "':='"
        | TokLParen -> "'('"
        | TokRParen -> "')'"
        | TokLBracket -> "'['"
        | TokRBracket -> "']'"
        | TokComma -> "','"
        | TokDot -> "'.'"
        | TokSemicolon -> "';'"
        | TokOp o -> $"'{o}'"
        | TokEnd -> "end of input"

type Located = { Token: Token; Pos: Pos }

type SyntaxError =
    { Message: string
      Pos: Pos }

    override e.ToString() = $"{e.Pos}: {e.Message}"

exception internal SyntaxException of SyntaxError

[<RequireQualifiedAccess>]
module Lexer =

    let keywords =
        set [ "if"; "then"; "elif"; "else"; "and"; "or"; "not"; "in"; "true"; "false" ]

    let private isIdentStart c = Char.IsAsciiLetter c || c = '_'
    let private isIdentChar c = Char.IsAsciiLetterOrDigit c || c = '_'

    /// Turns source text into tokens. Comments run from `//` to end of line. Numbers may use `_`
    /// as a digit separator and a trailing `%` divides by 100, so `7.5%` is 0.075.
    let tokenize (source: string) : Result<Located list, SyntaxError> =
        let tokens = ResizeArray<Located>()
        let mutable i = 0
        let mutable line = 1
        let mutable col = 1

        let peekAt k =
            if i + k < source.Length then source[i + k] else '\000'

        let advance () =
            if source[i] = '\n' then
                line <- line + 1
                col <- 1
            else
                col <- col + 1

            i <- i + 1

        let fail pos msg =
            raise (SyntaxException { Message = msg; Pos = pos })

        let emit pos tok = tokens.Add { Token = tok; Pos = pos }

        try
            while i < source.Length do
                let c = source[i]
                let pos = { Line = line; Column = col }

                if Char.IsWhiteSpace c then
                    advance ()
                elif c = '/' && peekAt 1 = '/' then
                    while i < source.Length && source[i] <> '\n' do
                        advance ()
                elif Char.IsAsciiDigit c then
                    let sb = StringBuilder()

                    let mutable seenDot = false

                    while i < source.Length
                          && (Char.IsAsciiDigit source[i]
                              || source[i] = '_'
                              || (source[i] = '.' && not seenDot && Char.IsAsciiDigit(peekAt 1))) do
                        if source[i] = '.' then
                            seenDot <- true

                        if source[i] <> '_' then
                            sb.Append(source[i]) |> ignore

                        advance ()

                    let text = sb.ToString()

                    let value =
                        match
                            Decimal.TryParse(
                                text,
                                NumberStyles.AllowDecimalPoint,
                                CultureInfo.InvariantCulture
                            )
                        with
                        | true, v -> v
                        | _ -> fail pos $"invalid number '{text}'"

                    if i < source.Length && source[i] = '%' then
                        advance ()
                        emit pos (TokNumber(value / 100m))
                    else
                        emit pos (TokNumber value)

                    if i < source.Length && isIdentStart source[i] then
                        fail { Line = line; Column = col } "a number must not run straight into a name"
                elif c = '"' then
                    advance ()
                    let sb = StringBuilder()
                    let mutable closed = false

                    while not closed do
                        if i >= source.Length || source[i] = '\n' then
                            fail pos "unterminated string literal"

                        match source[i] with
                        | '"' ->
                            advance ()
                            closed <- true
                        | '\\' ->
                            advance ()

                            if i >= source.Length then
                                fail pos "unterminated string literal"

                            match source[i] with
                            | '"' -> sb.Append('"') |> ignore
                            | '\\' -> sb.Append('\\') |> ignore
                            | 'n' -> sb.Append('\n') |> ignore
                            | 't' -> sb.Append('\t') |> ignore
                            | other ->
                                fail { Line = line; Column = col } $"unknown escape sequence '\\{other}'"

                            advance ()
                        | other ->
                            sb.Append(other) |> ignore
                            advance ()

                    emit pos (TokString(sb.ToString()))
                elif c = '@' then
                    advance ()
                    let start = i

                    while i < source.Length && (Char.IsAsciiDigit source[i] || source[i] = '-') do
                        advance ()

                    let text = source.Substring(start, i - start)

                    match Value.parseDate text with
                    | Some d -> emit pos (TokDate d)
                    | None -> fail pos $"invalid date literal '@{text}' (expected @yyyy-MM-dd)"
                elif c = '#' then
                    advance ()

                    if i >= source.Length || not (isIdentStart source[i]) then
                        fail pos "expected a table name after '#'"

                    let start = i

                    while i < source.Length && isIdentChar source[i] do
                        advance ()

                    emit pos (TokTable(source.Substring(start, i - start)))
                elif isIdentStart c then
                    let start = i

                    while i < source.Length && isIdentChar source[i] do
                        advance ()

                    let word = source.Substring(start, i - start)

                    emit
                        pos
                        (if keywords.Contains word then
                             TokKeyword word
                         else
                             TokIdent word)
                else
                    let two = if i + 1 < source.Length then source.Substring(i, 2) else ""

                    match two with
                    | ":=" ->
                        advance ()
                        advance ()
                        emit pos TokAssign
                    | "=="
                    | "!="
                    | "<="
                    | ">=" ->
                        advance ()
                        advance ()
                        emit pos (TokOp two)
                    | _ ->
                        match c with
                        | '(' -> emit pos TokLParen
                        | ')' -> emit pos TokRParen
                        | '[' -> emit pos TokLBracket
                        | ']' -> emit pos TokRBracket
                        | ',' -> emit pos TokComma
                        | '.' -> emit pos TokDot
                        | ';' -> emit pos TokSemicolon
                        | '+'
                        | '-'
                        | '*'
                        | '/'
                        | '<'
                        | '>' -> emit pos (TokOp(string c))
                        | '=' -> fail pos "use ':=' to define a step and '==' to compare"
                        | other -> fail pos $"unexpected character '{other}'"

                        advance ()

            emit { Line = line; Column = col } TokEnd
            Ok(List.ofSeq tokens)
        with SyntaxException e ->
            Error e
