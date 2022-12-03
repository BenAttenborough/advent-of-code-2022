module Advent1 exposing (..)

import Advent1Data exposing (..)
import Html exposing (b)
import List exposing (foldl)
import Maybe.Extra as Maybe
import Parser exposing (..)


final : String
final =
    test


valueParser : Parser Int
valueParser =
    succeed identity
        |= int
        |. end


result : Result (List Parser.DeadEnd) Int
result =
    Parser.run valueParser "1234 567"


valuesParser : Parser (List Int)
valuesParser =
    loop [] valuesParserHelp


valuesParserHelp : List Int -> Parser (Step (List Int) (List Int))
valuesParserHelp list =
    oneOf
        [ succeed (\int -> Loop (int :: list))
            |= int
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse list))
        ]


resultTwo : Result (List Parser.DeadEnd) (List Int)
resultTwo =
    Parser.run valuesParser test


block : Parser (List Int)
block =
    Parser.sequence
        { start = ""
        , separator = "/n"
        , end = "/n/n"
        , spaces = spaces
        , item = int
        , trailing = Optional -- demand a trailing semi-colon
        }


resultThree : Result (List Parser.DeadEnd) (List Int)
resultThree =
    Parser.run block test


thoughtSeq : String
thoughtSeq =
    "1234m5678m9012mm3456m7891"


convertToList : String -> List String
convertToList string =
    String.lines string



-- finaliseTest : List (Maybe Int)


blockTwo : Parser (List Int)
blockTwo =
    Parser.sequence
        { start = "[,"
        , separator = ","
        , end = "]"
        , spaces = spaces
        , item = int
        , trailing = Optional -- demand a trailing semi-colon
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


finaliseTest =
    String.lines realData
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



-- |> Result.map
--     (foldl
--         (\a b ->
--             if b > a then
--                 b
--             else
--                 a
--         )
--         0
--     )
-- |> Parser.run blockTwo
-- |> List.map String.toInt
-- |> foldl
--     (\a b ->
--         case a of
--             [] ->
--                 a
--             _ ->
--                 a
--     )
--     []
-- var : Parser String
-- var =
--     getChompedString <|
--         succeed ()
--             |. chompIf Char.isDigit
--             |. chompWhile Char.isDigit


intsSeperatedByNumbers =
    succeed String.toInt
        |= (getChompedString <| chompWhile Char.isDigit)
        |. spaces
        |. symbol "m"
        |. spaces


newTest =
    Parser.run intsSeperatedByNumbers "1234m5678m9012mm3456m7891"


statements : Parser (List String)
statements =
    loop [] statementsHelp


statementsHelp : List String -> Parser (Step (List String) (List String))
statementsHelp revStmts =
    oneOf
        [ succeed (\stmt -> Loop (stmt :: revStmts))
            |= (getChompedString <| chompWhile Char.isDigit)
            |. spaces
            |. symbol "m"
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse revStmts))
        ]


textX =
    Parser.run statements "1234m5678m9012mm3456m7891m"
