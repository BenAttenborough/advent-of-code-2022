module Parsers.Parsers exposing (..)

import Parser exposing (..)



-- See ParsersTests for examples


listParser : Parser (List String)
listParser =
    loop [] listParserStep


zeroOrMore : (Char -> Bool) -> Parser String
zeroOrMore isOk =
    succeed ()
        |. chompWhile isOk
        |> getChompedString


oneOrMore : (Char -> Bool) -> Parser String
oneOrMore isOk =
    succeed ()
        |. chompIf isOk
        |. chompWhile isOk
        |> getChompedString


isNewLine : Char -> Bool
isNewLine char =
    char == '\n'


listParserStep : List String -> Parser (Step (List String) (List String))
listParserStep entries =
    let
        finish : String -> (List String -> Step (List String) (List String)) -> Step (List String) (List String)
        finish entry next =
            next (entry :: entries)
    in
    succeed finish
        |. symbol "- "
        |= zeroOrMore (not << isNewLine)
        |= oneOf
            [ succeed Loop
                |. symbol "\n"
            , succeed (Done << List.reverse)
                |. end
            ]
