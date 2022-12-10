module Advent1 exposing (..)


advent1Part1 input =
    input
        |> String.split "\n\n"
        |> List.map (String.split "\n")
        |> List.map (List.filterMap String.toInt)
        |> List.map List.sum
        |> List.maximum
