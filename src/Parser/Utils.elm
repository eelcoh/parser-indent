module Parser.Utils exposing (spaces, whitespace)

import Parser as P exposing ((|.), (|=), Parser, Problem, end, int, keyword, lazy, oneOf, succeed, symbol)



--
-- maybe : Parser a -> Parser (Maybe a)
-- maybe p =
--     oneOf
--         [ P.map Just p
--         , succeed Nothing
--         ]
--
--
-- maybeWithdefault : a -> Parser a -> Parser a
-- maybeWithdefault a p =
--     oneOf
--         [ p
--         , succeed a
--         ]
--
--
-- strings : Parser (List String)
-- strings =
--     P.sequence
--         { start = "["
--         , separator = ","
--         , end = "]"
--         , spaces = spaces
--         , item = string
--         , trailing = P.Forbidden
--         }
--


space : Parser ()
space =
    P.chompIf isSpace



-- spaces : Parser ()
-- spaces =
--     P.chompWhile isSpace


spaces : Parser ()
spaces =
    let
        step _ =
            P.oneOf
                [ P.succeed (P.Loop ())
                    |. P.chompIf isSpace
                , P.succeed (P.Done ())
                ]
    in
    P.loop () step



--
--
-- spaces1 : Parser ()
-- spaces1 =
--     succeed ()
--         |. space
--         |. spaces


whitespace : Parser ()
whitespace =
    let
        step _ =
            P.oneOf
                [ P.succeed (P.Loop ())
                    |. P.chompIf isSpace
                , P.succeed (P.Loop ())
                    |. P.chompIf isNewLine
                , P.succeed (P.Done ())
                ]
    in
    P.loop () step


isSpace : Char -> Bool
isSpace =
    is ' '


isNewLine : Char -> Bool
isNewLine =
    is '\n'


is : Char -> Char -> Bool
is searched char =
    char == searched



--
--
-- eol : Parser ()
-- eol =
--     P.chompUntilEndOr "\n"
--
--
-- string : Parser String
-- string =
--     succeed ()
--         |. P.chompWhile isStringLiteral
--         |> P.getChompedString
--
--
-- isStringLiteral : Char -> Bool
-- isStringLiteral c =
--     Char.isDigit c
--         || Char.isUpper c
--         || Char.isLower c
--
--
-- isSentenceLiteral : Char -> Bool
-- isSentenceLiteral c =
--     isSpace c
--         || Char.isDigit c
--         || Char.isUpper c
--         || Char.isLower c
--
--
-- doubleQuotes : P.Parser String
-- doubleQuotes =
--     P.succeed identity
--         |. P.symbol "\""
--         |= characters_ (not << isDoubleQuote)
--         |. P.symbol "\""
--
--
-- {-| -}
-- characters_ : (Char -> Bool) -> P.Parser String
-- characters_ isOk =
--     P.succeed ()
--         |. P.chompWhile isOk
--         |> P.getChompedString
--
--
-- doubleQuote : Char
-- doubleQuote =
--     '"'
--
--
-- isDoubleQuote : Char -> Bool
-- isDoubleQuote char =
--     char == doubleQuote
