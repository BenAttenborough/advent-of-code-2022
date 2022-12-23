module Advent8 exposing (..)

import Advent8Data exposing (day8Part1Data, day8TestData)
import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra as List
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
        |> List.map (List.map (\tree -> ( tree, False )))
        |> List.map (findVisibleTrees -1 [])
        |> List.map List.reverse
        |> List.map (findVisibleTrees -1 [])
        |> List.map List.reverse
        |> Matrix.fromLists
        |> Maybe.map Matrix.transpose
        |> Maybe.map Matrix.toLists
        |> Maybe.map (List.map (findVisibleTrees -1 []))
        |> Maybe.map (List.map List.reverse)
        |> Maybe.map (List.map (findVisibleTrees -1 []))
        |> Maybe.map (List.map (List.filter Tuple.second))
        |> Maybe.map (List.map List.length)
        |> Maybe.map List.sum


stringToGrid : String -> List (List ( Int, Bool ))
stringToGrid input =
    input
        |> String.lines
        |> List.map String.toList
        |> List.map (List.filterMap charToInt)
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



---- PART 2


type alias TreesSeen =
    { north : Int
    , south : Int
    , east : Int
    , west : Int
    }


type alias Tree =
    { height : Int
    , treesSeen : TreesSeen
    }


day8Part2 input =
    input
        |> String.lines
        |> List.map String.toList
        |> List.map (List.filterMap charToInt)
        |> List.map (List.map (\height -> Tree height (TreesSeen 0 0 0 0)))
        |> List.map treesIterator
        |> Matrix.fromLists
        |> Maybe.map Matrix.transpose
        |> Maybe.map Matrix.toLists
        |> Maybe.map (List.map treesIteratorNorthSouth)
        |> Maybe.map
            (List.map
                (List.map
                    (\tree ->
                        tree.treesSeen
                    )
                )
            )
        |> Maybe.map
            (List.map
                (List.map
                    (\treesSeen ->
                        treesSeen.north
                            * treesSeen.south
                            * treesSeen.east
                            * treesSeen.west
                    )
                )
            )
        |> Maybe.andThen Matrix.fromLists
        |> Maybe.map Matrix.transpose
        |> Maybe.map Matrix.toLists
        |> Maybe.map (List.map List.maximum)
        |> Maybe.map (List.filterMap identity)
        |> Maybe.andThen List.maximum
        |> Maybe.withDefault -1


next : List a -> Maybe a
next list =
    case list of
        [] ->
            Nothing

        _ :: tail ->
            List.head tail


seenTrees : List Tree -> Maybe Int
seenTrees trees =
    -- given a List of trees count how many trees can be seen to right of head
    Maybe.map2
        (\firstTree otherTrees ->
            List.stoppableFoldl
                (\nextTree acc ->
                    if nextTree.height >= firstTree.height then
                        List.Stop (acc + 1)

                    else
                        List.Continue (acc + 1)
                )
                0
                otherTrees
        )
        (List.head trees)
        (List.tail trees)


treesIterator =
    \trees ->
        let
            rowScenicValues : Array Int
            rowScenicValues =
                treeRowIterator seenTrees [] trees
                    |> Array.fromList

            rowScenicValuesReverse : Array Int
            rowScenicValuesReverse =
                treeRowIterator seenTrees [] (List.reverse trees)
                    |> List.reverse
                    |> Array.fromList
        in
        List.indexedMap
            (\index tree ->
                Tree tree.height
                    (TreesSeen
                        tree.treesSeen.north
                        tree.treesSeen.south
                        (Maybe.withDefault 1 (Array.get index rowScenicValues))
                        (Maybe.withDefault 1 (Array.get index rowScenicValuesReverse))
                    )
            )
            trees


treesIteratorNorthSouth =
    \trees ->
        let
            rowScenicValues : Array Int
            rowScenicValues =
                treeRowIterator seenTrees [] trees
                    |> Array.fromList

            rowScenicValuesReverse =
                treeRowIterator seenTrees [] (List.reverse trees)
                    |> List.reverse
                    |> Array.fromList
        in
        List.indexedMap
            (\index tree ->
                Tree tree.height
                    (TreesSeen
                        (Maybe.withDefault 1 (Array.get index rowScenicValues))
                        (Maybe.withDefault 1 (Array.get index rowScenicValuesReverse))
                        tree.treesSeen.east
                        tree.treesSeen.west
                    )
            )
            trees


treeRowIterator : (List Tree -> Maybe Int) -> List (Maybe Int) -> List Tree -> List Int
treeRowIterator fnc initialTrees trees =
    case trees of
        [] ->
            initialTrees
                |> List.reverse
                |> List.filterMap identity

        head :: tail ->
            treeRowIterator fnc (fnc trees :: initialTrees) tail


listTreeAoCExampleX : List Tree
listTreeAoCExampleX =
    [ { height = 3, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    , { height = 3, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    , { height = 5, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    , { height = 4, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    , { height = 9, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    ]


view model =
    div []
        [ pre [ style "white-space" "pre-line" ]
            [ text "TEST\n\n"

            -- , text <| Debug.toString (day8Part2 day8TestData)
            -- , text <| "\n"
            -- , text <| Debug.toString (treeRowIterator seenTrees [] listTreeAoCExampleX)
            , text <| Debug.toString (day8Part2 day8TestData)

            -- , text <| Debug.toString (day8Part2 day8Part1Data)
            -- , text <| Debug.toString listTreeAoCExampleX
            ]
        ]


main =
    view "dummy model"


testRow =
    [ { height = 3, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    , { height = 3, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    , { height = 5, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    , { height = 4, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    , { height = 9, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    ]
