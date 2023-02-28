module Advent1 exposing (..)

import Html


main : Html.Html msg
main =
    Html.text "Hello!"


advent1Part1 : String -> Maybe Int
advent1Part1 input =
    input
        |> String.split "\n\n"
        |> List.map (String.split "\n")
        |> List.map (List.filterMap String.toInt)
        |> List.map List.sum
        |> List.maximum
