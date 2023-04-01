module Advent12 exposing (..)

import Array exposing (Array)
import Char exposing (toCode)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra as List
import Set exposing (Set)
import Tree exposing (tree)
import Tree.Zipper exposing (Zipper, append)
import Tuple3
import Utilities.Utilities exposing (uniqueItemFrom2DArray)


type Tile
    = Start
    | Journey
    | End


type alias Cell =
    ( Int, Int, Int )


type alias Position =
    { x : Int
    , y : Int
    }


getNode : Int -> Int -> Array (Array a) -> Maybe a
getNode x y twoDMap =
    twoDMap
        |> Array.get y
        |> Maybe.andThen (Array.get x)


aCode : number
aCode =
    97


startChar : Char
startChar =
    'S'


endChar : Char
endChar =
    'E'


startCharCode : Int
startCharCode =
    toCode startChar


endCharCode : Int
endCharCode =
    toCode endChar


startElevation : Int
startElevation =
    0


endElevation : Int
endElevation =
    25


charToCode : Char -> Int
charToCode =
    toCode
        >> (\code ->
                if code == startCharCode then
                    startElevation

                else if code == endCharCode then
                    endElevation

                else
                    code - aCode
           )


charToCellType : Char -> Tile
charToCellType char =
    case char of
        'S' ->
            Start

        'E' ->
            End

        _ ->
            Journey


convertCellToKey : Cell -> String
convertCellToKey cell =
    let
        x =
            String.padLeft 2 '0' (String.fromInt (cellX cell))

        y =
            String.padLeft 2 '0' (String.fromInt (cellY cell))
    in
    x ++ "-" ++ y


cellX : Cell -> Int
cellX =
    Tuple3.first


cellY : Cell -> Int
cellY =
    Tuple3.second


cellZ : Cell -> Int
cellZ =
    Tuple3.third


nodeTraversable : Cell -> Cell -> Maybe Cell
nodeTraversable currentCell nextCell =
    let
        predicate =
            cellZ nextCell <= (cellZ currentCell + 1) && cellZ nextCell >= (cellZ currentCell - 1)
    in
    if predicate then
        Just nextCell

    else
        Nothing


getAvailableNeighbours : Cell -> Array (Array Cell) -> String -> List String
getAvailableNeighbours cell atlas parentKey =
    let
        up =
            getNode (cellX cell) (cellY cell - 1) atlas
                |> Maybe.andThen (nodeTraversable cell)

        down =
            getNode (cellX cell) (cellY cell + 1) atlas
                |> Maybe.andThen (nodeTraversable cell)

        left =
            getNode (cellX cell - 1) (cellY cell) atlas
                |> Maybe.andThen (nodeTraversable cell)

        right =
            getNode (cellX cell + 1) (cellY cell) atlas
                |> Maybe.andThen (nodeTraversable cell)
    in
    [ up, down, left, right ]
        |> List.filterMap identity
        |> List.map convertCellToKey
        |> List.filter (\item -> not (item == parentKey))


getAvailableNeighboursCell : Cell -> Array (Array Cell) -> List Cell
getAvailableNeighboursCell cell atlas =
    let
        up =
            getNode (cellX cell) (cellY cell - 1) atlas
                |> Maybe.andThen (nodeTraversable cell)

        down =
            getNode (cellX cell) (cellY cell + 1) atlas
                |> Maybe.andThen (nodeTraversable cell)

        left =
            getNode (cellX cell - 1) (cellY cell) atlas
                |> Maybe.andThen (nodeTraversable cell)

        right =
            getNode (cellX cell + 1) (cellY cell) atlas
                |> Maybe.andThen (nodeTraversable cell)
    in
    [ up, down, left, right ]
        |> List.filterMap identity


findStart : String -> Maybe Cell
findStart stringInput =
    stringInput
        |> inputToCharArray
        |> uniqueItemFrom2DArray (\item -> Tuple3.third item == 'S')


findEnd : String -> Maybe Cell
findEnd stringInput =
    stringInput
        |> inputToCharArray
        |> uniqueItemFrom2DArray (\item -> Tuple3.third item == 'E')


inputToCharArray : String -> Array (Array ( Int, Int, Char ))
inputToCharArray =
    String.lines
        >> List.map
            (String.toList
                >> List.map
                    (\char ->
                        ( 0, 0, char )
                    )
                >> Array.fromList
                >> Array.indexedMap (\index ( _, y, z ) -> ( index, y, z ))
            )
        >> Array.fromList
        >> Array.indexedMap (\index arr -> Array.map (\( x, _, z ) -> ( x, index, z )) arr)


prepareInput : String -> Array (Array Cell)
prepareInput =
    String.lines
        >> List.map
            (String.toList
                >> List.map
                    (\char ->
                        ( 0, 0, charToCode char )
                    )
                >> Array.fromList
                >> Array.indexedMap (\index ( _, y, z ) -> ( index, y, z ))
            )
        >> Array.fromList
        >> Array.indexedMap (\index arr -> Array.map (\( x, _, z ) -> ( x, index, z )) arr)


view : Html msg
view =
    div []
        [ pre [ style "white-space" "pre-line" ]
            [ text "TEST\n\n"
            ]
        ]


countStepsToEnd : List Cell -> Cell -> Array (Array Cell) -> List Cell -> Int -> Int
countStepsToEnd queue endCell atlas visited count =
    if List.any (\cell -> cell == endCell) queue then
        count

    else
        let
            allNeighbours =
                List.map (\cell -> getAvailableNeighboursCell cell atlas) queue
                    |> List.concat
                    |> List.filter (\item -> not (List.member item visited))
                    |> List.unique
                    |> Debug.log "All Neighbours"

            -- Debug.log "All Neighbours"
            --     (List.unique (List.filter (\item -> not (List.member item visited)) (List.concat (List.map (\cell -> getAvailableNeighboursCell cell atlas) queue))))
            -- Debug.log "All Neighbours"
            --     (List.unique (List.filter (\item -> not (List.member item visited)) (List.concat (List.map (\cell -> getAvailableNeighboursCell cell atlas) queue))))
            updatedVisited =
                visited
                    ++ allNeighbours
                    |> List.unique

            -- visited ++ allNeighbours
            updatedCount =
                count + 1
        in
        if List.length allNeighbours == 0 then
            -1

        else
            countStepsToEnd allNeighbours endCell atlas updatedVisited updatedCount


part1Solution : String -> Maybe Int
part1Solution input =
    let
        atlas =
            input
                |> prepareInput

        start =
            findStart input

        end =
            findEnd input
    in
    Maybe.map (\begin -> countStepsToEnd [ begin ] end atlas [ begin ] 0) start


main : Html msg
main =
    view
