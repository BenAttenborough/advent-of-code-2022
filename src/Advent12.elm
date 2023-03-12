module Advent12 exposing (..)

-- import Data.Advent12Data exposing (testInput)

import Array exposing (Array)
import Char exposing (toCode)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
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


simpleArrayTarverse : Int -> Array a -> Array a -> Array a
simpleArrayTarverse index new old =
    case Array.get index old of
        Just value ->
            simpleArrayTarverse (index + 1) (Array.push value new) old

        Nothing ->
            new


getNodes : Int -> Int -> Array (Array a) -> List a
getNodes orgX orgY twoDMap =
    let
        up =
            getNode orgX (orgY - 1) twoDMap

        down =
            getNode orgX (orgY + 1) twoDMap

        left =
            getNode (orgX - 1) orgY twoDMap

        right =
            getNode (orgX + 1) orgY twoDMap
    in
    [ up, down, left, right ]
        |> List.filterMap identity


getNode : Int -> Int -> Array (Array a) -> Maybe a
getNode x y twoDMap =
    twoDMap
        |> Array.get y
        |> Maybe.andThen (Array.get x)


simpleArrayTarverseTwo : Int -> Zipper a -> Array a -> Zipper a
simpleArrayTarverseTwo index tree atlas =
    case Array.get index atlas of
        Just value ->
            simpleArrayTarverseTwo (index + 1) (append (Tree.singleton value) tree) atlas

        Nothing ->
            tree


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


type alias GraphNode =
    { key : String
    , neighbours : List String
    , destination : Tile
    }


type alias Graph =
    Dict String GraphNode


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


getNodesIfTravesable : Cell -> Array (Array Cell) -> List Cell
getNodesIfTravesable cell twoDMap =
    let
        up =
            getNode cell.x (cell.y - 1) twoDMap
                |> Maybe.andThen (nodeTraversable cell)

        down =
            getNode cell.x (cell.y + 1) twoDMap
                |> Maybe.andThen (nodeTraversable cell)

        left =
            getNode (cell.x - 1) cell.y twoDMap
                |> Maybe.andThen (nodeTraversable cell)

        right =
            getNode (cell.x + 1) cell.y twoDMap
                |> Maybe.andThen (nodeTraversable cell)
    in
    [ up, down, left, right ]
        |> List.filterMap identity


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


convertCellToNode : Cell -> Array (Array Cell) -> String -> GraphNode
convertCellToNode cell atlas parent =
    let
        key =
            convertCellToKey cell

        neighbours =
            getAvailableNeighbours cell atlas parent

        destination =
            cell.cellType
    in
    GraphNode key neighbours destination



-- buildGraph : Cell -> Array (Array Cell) -> String -> SGraph -> SGraph
-- buildGraph cell atlas parent graph =
--     let
--         neighbours =
--             getAvailableNeighbours cell atlas parent
--     in
--     case neighbours of
--         [] ->
--             Dict.insert (convertCellToKey cell) { neighbours = neighbours, destination = cell.cellType } graph
--         x :: xs ->
--             let
--                 neighbours_ =
--                     []
--             in
--             Dict.insert (convertCellToKey cell) { neighbours = neighbours_, destination = cell.cellType } graph


cellListToNeighboursList : Array (Array Cell) -> List ( String, GraphNode ) -> List String -> List Cell -> List ( String, GraphNode )
cellListToNeighboursList arr container alreadyTraversed cellList =
    case cellList of
        [] ->
            container

        cell :: cells ->
            let
                cellKey =
                    convertCellToKey cell

                traversed =
                    alreadyTraversed
                        ++ [ cellKey ]

                -- |> Debug.log "Traversed"
                neighbours =
                    -- Need to avoid adding cells already traveresed
                    getNodesIfTravesable cell arr
                        |> List.map convertCellToKey
                        |> List.filter (\key -> not (List.member key traversed))

                newGraphNode =
                    ( convertCellToKey cell
                    , { key = convertCellToKey cell
                      , neighbours = neighbours
                      , destination = cell.cellType
                      }
                    )

                newGraphNodeList =
                    container ++ [ newGraphNode ]
            in
            cellListToNeighboursList arr newGraphNodeList traversed cells


cellArrayToCellGraph : Array (Array Cell) -> Graph
cellArrayToCellGraph arr =
    -- Need to adjust this
    arr
        |> Array.map Array.toList
        |> Array.toList
        |> List.concat
        -- Need a cleaver function where we only add neighbours that have not already been traversed
        |> cellListToNeighboursList arr [] []
        |> Dict.fromList


findStart : Array (Array Cell) -> Maybe Cell
findStart atlas =
    let
        predicate =
            \cell -> cell.cellType == Start
    in
    uniqueItemFrom2DArray predicate atlas


removeNonUniqueValues : List a -> List a -> List a
removeNonUniqueValues list uniqueValues =
    let
        fnc : a -> List a -> List a
        fnc value accumulator =
            if List.member value uniqueValues then
                accumulator

            else
                accumulator ++ [ value ]
    in
    List.foldl fnc [] list


countNodesToEnd : Int -> Graph -> List String -> Int
countNodesToEnd currentCount graph nodes =
    case nodes of
        [] ->
            -1

        x :: xs ->
            let
                node =
                    getNodeFromGraph x graph

                uniqueNodesToAdd =
                    removeNonUniqueValues node.neighbours xs

                -- |> Debug.log "uniqueNodesToAdd"
                nodesToAdd =
                    xs
                        ++ uniqueNodesToAdd

                -- |> Debug.log "nodesToAdd"
            in
            if graphNodeIsEnd node then
                currentCount + 1

            else
                -- let
                --     _ =
                --         Debug.log "count" currentCount
                --     a =
                --         Debug.log "nodesToAdd" nodesToAdd
                -- in
                countNodesToEnd (currentCount + 1) graph nodesToAdd



-- Need set like behaviour, but must be ordered


graphNodeIsEnd : GraphNode -> Bool
graphNodeIsEnd node =
    node.destination == End


getNodeFromGraph : String -> Graph -> GraphNode
getNodeFromGraph key graph =
    -- Dodgy default - but should be impossible to get to
    Dict.get key graph
        |> Maybe.withDefault { key = "", neighbours = [], destination = Journey }


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


countSteps : Cell -> Array (Array Cell) -> List Cell -> Int -> Int
countSteps cell atlas queue count =
    if cell.cellType == End then
        count

    else
        let
            neighbours : List Cell
            neighbours =
                getAvailableNeighboursCell cell atlas

            updatedQueue : List Cell
            updatedQueue =
                queue ++ neighbours
        in
        -1


part1Solution : String -> Maybe Cell
part1Solution input =
    input
        |> prepareInput
        |> findStart



-- |> Maybe.map
-- part1Solution : String -> Int
-- part1Solution input =
--     let
--         graph =
--             input
--                 |> prepareInput
--                 |> cellArrayToCellGraph
--     in
--     graph
--         |> findStart
--         |> Maybe.map (\start -> start.neighbours ++ [ start.key ])
--         |> Maybe.withDefault []
--         |> countNodesToEnd 0 graph


main : Html msg
main =
    view
