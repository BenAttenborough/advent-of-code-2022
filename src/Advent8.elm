module Advent8 exposing (..)

import Advent8Data exposing (day8Part1Data)
import Matrix exposing (fromLists, transpose)


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
    let
        output =
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
                |> Debug.log ">"
    in
    list


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
