module Advent12 exposing (..)

-- import Data.Advent12Data exposing (testInput)

import Array exposing (Array)
import Char exposing (toCode)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra as List
import Tree exposing (tree)
import Tree.Zipper exposing (Zipper, append)
import Utilities.Utilities exposing (uniqueItemFrom2DArray)


type Tile
    = Start
    | Journey
    | End


type alias Cell =
    { elevation : Int
    , cellType : Tile
    , x : Int
    , y : Int
    }


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
            String.padLeft 2 '0' (String.fromInt cell.x)

        y =
            String.padLeft 2 '0' (String.fromInt cell.y)
    in
    x ++ "-" ++ y


nodeTraversable : Cell -> Cell -> Maybe Cell
nodeTraversable currentCell nextCell =
    let
        predicate =
            nextCell.elevation <= (currentCell.elevation + 1) && nextCell.elevation >= (currentCell.elevation - 1)
    in
    if predicate then
        Just nextCell

    else
        Nothing


getAvailableNeighbours : Cell -> Array (Array Cell) -> String -> List String
getAvailableNeighbours cell atlas parentKey =
    let
        up =
            getNode cell.x (cell.y - 1) atlas
                |> Maybe.andThen (nodeTraversable cell)

        down =
            getNode cell.x (cell.y + 1) atlas
                |> Maybe.andThen (nodeTraversable cell)

        left =
            getNode (cell.x - 1) cell.y atlas
                |> Maybe.andThen (nodeTraversable cell)

        right =
            getNode (cell.x + 1) cell.y atlas
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
            getNode cell.x (cell.y - 1) atlas
                |> Maybe.andThen (nodeTraversable cell)

        down =
            getNode cell.x (cell.y + 1) atlas
                |> Maybe.andThen (nodeTraversable cell)

        left =
            getNode (cell.x - 1) cell.y atlas
                |> Maybe.andThen (nodeTraversable cell)

        right =
            getNode (cell.x + 1) cell.y atlas
                |> Maybe.andThen (nodeTraversable cell)
    in
    [ up, down, left, right ]
        |> List.filterMap identity


findStart : Array (Array Cell) -> Maybe Cell
findStart atlas =
    let
        predicate =
            \cell -> cell.cellType == Start
    in
    uniqueItemFrom2DArray predicate atlas


prepareInput : String -> Array (Array Cell)
prepareInput =
    String.lines
        >> List.map
            (String.toList
                >> List.map
                    (\char ->
                        { elevation = charToCode char
                        , cellType = charToCellType char
                        , x = 0
                        , y = 0
                        }
                    )
                >> Array.fromList
                >> Array.indexedMap (\index arr -> { arr | x = index })
            )
        >> Array.fromList
        >> Array.indexedMap (\index arr -> Array.map (\arr_ -> { arr_ | y = index }) arr)


view : Html msg
view =
    div []
        [ pre [ style "white-space" "pre-line" ]
            [ text "TEST\n\n"
            ]
        ]


countStepsToEnd : List Cell -> Array (Array Cell) -> List Cell -> Int -> Int
countStepsToEnd queue atlas visited count =
    if List.any (\cell -> cell.cellType == End) queue then
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
            countStepsToEnd allNeighbours atlas updatedVisited updatedCount


part1Solution : String -> Maybe Int
part1Solution input =
    let
        atlas =
            input
                |> prepareInput

        start =
            findStart atlas
    in
    Maybe.map (\begin -> countStepsToEnd [ begin ] atlas [ begin ] 0) start


main : Html msg
main =
    view
