module Advent8 exposing (..)

import Advent8Data exposing (day8Part1Data, day8TestData)
import Html exposing (..)
import Html.Attributes exposing (..)
import Matrix


charToInt : Char -> Maybe Int
charToInt char =
    if Char.isDigit char then
        Just (Char.toCode char - 0x30)

    else
        Nothing


compareVerticalRows input =
    input


day8Part1 input =
    input
        |> String.lines
        |> List.map String.toList
        |> List.map (List.filterMap charToInt)
        -- Turn each tree into tuple. Second value used to indicate if tree has been counted
        -- I've just realised this probably isn't useful
        |> List.map (List.map (\tree -> ( tree, False )))
        -- |> outputGrid
        |> List.map (findVisibleTrees -1 [])
        -- |> outputGrid
        |> List.map List.reverse
        |> List.map (findVisibleTrees -1 [])
        |> List.map List.reverse
        -- |> outputGrid
        |> Matrix.fromLists
        |> Maybe.map Matrix.transpose
        |> Maybe.map Matrix.toLists
        |> Maybe.map (List.map (findVisibleTrees -1 []))
        |> Maybe.map (List.map List.reverse)
        -- |> Maybe.map outputGrid
        |> Maybe.map (List.map (findVisibleTrees -1 []))
        -- |> Maybe.map outputGrid
        -- |> Maybe.withDefault ""
        |> Maybe.map (List.map (List.filter Tuple.second))
        |> Maybe.map (List.map List.length)
        |> Maybe.map List.sum


stringToGrid : String -> List (List ( Int, Bool ))
stringToGrid input =
    input
        |> String.lines
        |> List.map String.toList
        |> List.map (List.filterMap charToInt)
        -- Turn each tree into tuple. Second value used to indicate if tree has been counted
        -- I've just realised this probably isn't useful
        |> List.map (List.map (\tree -> ( tree, False )))


outputGrid list =
    list
        |> List.map
            (List.map
                (\( height, tagged ) ->
                    if tagged then
                        1

                    else
                        0
                )
            )
        |> List.map (List.map String.fromInt)
        |> List.map (String.join "")
        |> List.intersperse "\n"
        |> String.join ""


findVisibleTrees : Int -> List ( Int, Bool ) -> List ( Int, Bool ) -> List ( Int, Bool )
findVisibleTrees currentHighest processedTrees trees =
    case trees of
        [] ->
            List.reverse processedTrees

        head :: rest ->
            if Tuple.first head > currentHighest then
                findVisibleTrees (Tuple.first head) (( Tuple.first head, True ) :: processedTrees) rest

            else
                findVisibleTrees currentHighest (head :: processedTrees) rest


testInput : String
testInput =
    """30373
25512
65332
33549
35390"""


type alias TreesSeen =
    { north : Int
    , south : Int
    , east : Int
    , west : Int
    }


type TreeHeight
    = TreeHeight Int


type alias Tree =
    { height : TreeHeight
    , treesSeen : TreesSeen
    }


day8Part2 input =
    input
        |> String.lines
        |> List.map String.toList
        |> List.map (List.filterMap charToInt)
        |> List.map (List.map (\height -> Tree (TreeHeight height) (TreesSeen 0 0 0 0)))


seenTrees : List Tree -> List Tree
seenTrees trees =
    trees



-- case trees of
--     [] ->
--         List.reverse processedTrees
--     head :: rest ->
--         if Tuple.first head > currentHighest then
--             findVisibleTrees (Tuple.first head) (( Tuple.first head, True ) :: processedTrees) rest
--         else
--             findVisibleTrees currentHighest (head :: processedTrees) rest


listTreeExample : List Tree
listTreeExample =
    [ { height = TreeHeight 3, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }, { height = TreeHeight 0, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }, { height = TreeHeight 3, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }, { height = TreeHeight 7, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }, { height = TreeHeight 3, treesSeen = { east = 0, north = 0, south = 0, west = 0 } } ]


view model =
    div []
        [ p []
            [ text "TEST\n"

            -- , text <| Debug.toString (day8Part2 day8TestData)
            , text <| Debug.toString (seenTrees listTreeExample)
            ]

        -- , pre [ Html.Attributes.style "white-space" "pre-wrap" ]
        --     [ text (day8Part1 day8TestData) ]
        ]


main =
    view "dummy model"
