module Advent2 exposing (..)

import Html exposing (a)
import Parser exposing (..)


testInput : String
testInput =
    """A Y
B X
C Z"""


type Attack
    = Rock
    | Paper
    | Scissors


attackParser : Parser Attack
attackParser =
    oneOf
        [ succeed Rock
            |. keyword "A"
        , succeed Paper
            |. keyword "B"
        , succeed Scissors
            |. keyword "C"
        ]


characterParser : Parser ()
characterParser =
    chompWhile (\c -> c == 'A' || c == 'B' || c == 'C')


defenseParser : Parser Attack
defenseParser =
    oneOf
        [ succeed Rock
            |. keyword "X"
        , succeed Paper
            |. keyword "Y"
        , succeed Scissors
            |. keyword "Z"
        ]


partOne input =
    input
        |> String.lines
        |> List.map (\x -> Tuple.pair (getAttack x) (getDefense (String.right 1 x)))
        |> List.map convertOks


getAttack string =
    Parser.run attackParser string


getDefense string =
    Parser.run defenseParser string


runAttackParser string =
    Parser.run attackParser string


convertOks tuple =
    Result.map2 (\a b -> ( a, b )) (Tuple.first tuple) (Tuple.second tuple)



-- case tuple of
--    (Ok a, Ok b) ->
--         Ok (a, b)
--     ( Err x, _ ) ->
--         Err x
--     ( _, Err x ) ->
--         Err x
-- see https://elmprogramming.com/pattern-matching.html
