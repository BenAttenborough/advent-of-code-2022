module Advent2 exposing (..)

import Html
import Parser exposing (..)
import Result exposing (toMaybe)


main : Html.Html msg
main =
    Html.text "Hello!"


testInput : String
testInput =
    """A Y
B X
C Z"""


type Action
    = Rock
    | Paper
    | Scissors


type Result
    = Win
    | Lose
    | Draw


rockValue : Int
rockValue =
    1


paperValue : Int
paperValue =
    2


scissorsValue : Int
scissorsValue =
    3


loseValue : Int
loseValue =
    0


drawValue : Int
drawValue =
    3


winValue : Int
winValue =
    6


type alias Combat =
    { attack : Action
    , defense : Action
    }


type alias CombatCheat =
    { attack : Action
    , response : Result
    }


resolveCombat : Combat -> Int
resolveCombat { attack, defense } =
    case attack of
        Rock ->
            case defense of
                Rock ->
                    rockValue + drawValue

                Paper ->
                    paperValue + winValue

                Scissors ->
                    scissorsValue + loseValue

        Paper ->
            case defense of
                Rock ->
                    rockValue + loseValue

                Paper ->
                    paperValue + drawValue

                Scissors ->
                    scissorsValue + winValue

        Scissors ->
            case defense of
                Rock ->
                    rockValue + winValue

                Paper ->
                    paperValue + loseValue

                Scissors ->
                    scissorsValue + drawValue


generalParser : Parser Combat
generalParser =
    succeed Combat
        |= oneOf
            [ succeed Rock
                |. keyword "A"
            , succeed Paper
                |. keyword "B"
            , succeed Scissors
                |. keyword "C"
            ]
        |. spaces
        |= oneOf
            [ succeed Rock
                |. keyword "X"
            , succeed Paper
                |. keyword "Y"
            , succeed Scissors
                |. keyword "Z"
            ]


attackParser : Parser Action
attackParser =
    oneOf
        [ succeed Rock
            |. keyword "A"
        , succeed Paper
            |. keyword "B"
        , succeed Scissors
            |. keyword "C"
        ]


defenseParser : Parser Action
defenseParser =
    oneOf
        [ succeed Rock
            |. keyword "X"
        , succeed Paper
            |. keyword "Y"
        , succeed Scissors
            |. keyword "Z"
        ]


partOne : String -> Int
partOne input =
    input
        |> String.lines
        |> List.filterMap
            (\string ->
                string
                    |> Parser.run generalParser
                    |> toMaybe
            )
        |> List.map resolveCombat
        |> List.sum


parseStringCheat : String -> Maybe CombatCheat
parseStringCheat string =
    let
        attack =
            Parser.run attackParser string
                |> toMaybe

        response =
            Parser.run resultParser (String.right 1 string)
                |> toMaybe
    in
    Maybe.map2 (\a b -> CombatCheat a b) attack response


resultParser : Parser Result
resultParser =
    oneOf
        [ succeed Lose
            |. keyword "X"
        , succeed Draw
            |. keyword "Y"
        , succeed Win
            |. keyword "Z"
        ]


resolveResult : CombatCheat -> Int
resolveResult { attack, response } =
    case attack of
        Rock ->
            case response of
                Win ->
                    paperValue + winValue

                Lose ->
                    scissorsValue + loseValue

                Draw ->
                    rockValue + drawValue

        Paper ->
            case response of
                Win ->
                    scissorsValue + winValue

                Lose ->
                    rockValue + loseValue

                Draw ->
                    paperValue + drawValue

        Scissors ->
            case response of
                Win ->
                    rockValue + winValue

                Lose ->
                    paperValue + loseValue

                Draw ->
                    scissorsValue + drawValue


partTwo : String -> Int
partTwo input =
    input
        |> String.lines
        |> List.filterMap parseStringCheat
        |> List.map resolveResult
        |> List.sum
