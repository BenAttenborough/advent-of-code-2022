module AlternativeSolutions.Advent1 exposing (..)

import Data.Advent1Data exposing (..)
import List exposing (foldl)
import Parser exposing (..)


blockTwo : Parser (List Int)
blockTwo =
    Parser.sequence
        { start = "[,"
        , separator = ","
        , end = "]"
        , spaces = spaces
        , item = int
        , trailing = Optional
        }


looper : Parser (List (List Int))
looper =
    loop [] loopHelper


loopHelper revStmts =
    oneOf
        [ succeed (\stmt -> Loop (stmt :: revStmts))
            |= blockTwo
            |. spaces
            |. symbol ";"
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse revStmts))
        ]


getBiggestRations data =
    String.lines data
        |> List.map
            (\string ->
                if string == "" then
                    "];["

                else
                    string
            )
        |> List.intersperse ","
        |> List.foldr String.append ""
        |> (++) "[,"
        |> (\string -> string ++ ",];")
        |> Parser.run looper
        |> Result.map (List.map (foldl (+) 0))
        |> Result.map
            (foldl
                (\a b ->
                    if b > a then
                        b

                    else
                        a
                )
                0
            )


getTopThreeRations data =
    String.lines data
        |> List.map
            (\string ->
                if string == "" then
                    "];["

                else
                    string
            )
        |> List.intersperse ","
        |> List.foldr String.append ""
        |> (++) "[,"
        |> (\string -> string ++ ",];")
        |> Parser.run looper
        |> Result.map (List.map (foldl (+) 0))
        |> Result.map List.sort
        |> Result.map List.reverse
        |> Result.map (List.take 3)
        |> Result.map List.sum


listParser : Parser (List String)
listParser =
    loop [] listParserStep


listParserStep : List String -> Parser (Step (List String) (List String))
listParserStep entries =
    let
        finish : String -> (List String -> Step (List String) (List String)) -> Step (List String) (List String)
        finish entry next =
            next (entry :: entries)
    in
    succeed finish
        |= zeroOrMore (not << isNewLine)
        |= oneOf
            [ succeed Loop
                |. symbol "\n"
            , succeed (Done << List.reverse)
                |. end
            ]


zeroOrMore : (Char -> Bool) -> Parser String
zeroOrMore isOk =
    succeed ()
        |. chompWhile isOk
        |> getChompedString


isNewLine : Char -> Bool
isNewLine char =
    char == '\n'


parseInput input =
    -- Ok [ 111 ]
    Parser.run listParser input



-- specialParser : Paser (List Int)
-- specialParser =
--     succeed identity
-- advent1Part1 : String -> List (Result (List DeadEnd) (List String))


advent1Part1 input =
    input
        |> String.split "\n\n"
        |> List.map
            (Parser.run listParser)
        |> List.map Result.toMaybe
        |> List.filterMap identity
        |> List.map (List.map String.toInt)
        |> List.map (List.filterMap identity)
        |> List.map List.sum
        |> List.maximum
        |> Maybe.withDefault 0
