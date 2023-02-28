module Advent4 exposing (..)

import Html
import Parser exposing (..)
import Set


main =
    Html.text "Hello!"


day4TestInput =
    """2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"""


type alias WorkRange =
    { worker1 : List Int
    , worker2 : List Int
    }


stringParser : Parser WorkRange
stringParser =
    succeed workRangeConstructor
        |= int
        |. symbol "-"
        |= int
        |. symbol ","
        |= int
        |. symbol "-"
        |= int


workRangeConstructor a b c d =
    let
        range1 =
            List.range a b

        range2 =
            List.range c d
    in
    WorkRange range1 range2


doesOneListContainOther a b =
    let
        setA =
            Set.fromList a

        setB =
            Set.fromList b

        diffA =
            Set.diff setA setB
                |> Set.toList

        diffB =
            Set.diff setB setA
                |> Set.toList
    in
    if List.isEmpty diffA then
        False

    else if List.isEmpty diffB then
        False

    else
        True


day4Part1 input =
    input
        |> String.lines
        |> List.map (Parser.run stringParser)
        |> Debug.log "da"
        |> List.map (Result.map (\list -> doesOneListContainOther list.worker1 list.worker2))
        |> List.map Result.toMaybe
        |> List.filterMap identity
        |> List.filter identity
        |> List.length


doesOneListOverlapOther a b =
    let
        setA =
            Set.fromList a

        setB =
            Set.fromList b

        diffA =
            Set.intersect setA setB
                |> Set.toList

        diffB =
            Set.intersect setB setA
                |> Set.toList
    in
    if not (List.isEmpty diffA) then
        True

    else if not (List.isEmpty diffB) then
        True

    else
        False


day4Part2 input =
    input
        |> String.lines
        |> List.map (Parser.run stringParser)
        |> List.map (Result.map (\list -> doesOneListOverlapOther list.worker1 list.worker2))
        |> List.map Result.toMaybe
        |> List.filterMap identity
        |> List.filter identity
        |> List.length
