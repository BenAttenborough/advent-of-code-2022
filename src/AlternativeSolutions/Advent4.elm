module AlternativeSolutions.Advent4 exposing (..)

import Parser exposing (..)


day4TestInput : String
day4TestInput =
    """2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"""


stringParser : Parser ( ( Int, Int ), ( Int, Int ) )
stringParser =
    succeed workRangeConstructor
        |= int
        |. symbol "-"
        |= int
        |. symbol ","
        |= int
        |. symbol "-"
        |= int


workRangeConstructor : a -> b -> c -> d -> ( ( a, b ), ( c, d ) )
workRangeConstructor a b c d =
    ( ( a, b ), ( c, d ) )


doesContain : ( ( Int, Int ), ( Int, Int ) ) -> Int
doesContain ( ( a, b ), ( c, d ) ) =
    if ((a <= c) && (b >= d)) || ((a >= c) && (b <= d)) then
        1

    else
        0


day4Part1 : String -> Int
day4Part1 input =
    input
        |> String.lines
        |> List.map (Parser.run stringParser)
        |> List.map Result.toMaybe
        |> List.filterMap identity
        |> List.map doesContain
        |> List.sum



-- day4Part2 input =
--     input
--         |> String.lines
--         |> List.map (Parser.run stringParser)
--         |> List.map (Result.map (\list -> doesOneListOverlapOther list.worker1 list.worker2))
--         |> List.map Result.toMaybe
--         |> List.filterMap identity
--         |> List.filter identity
--         |> List.length
