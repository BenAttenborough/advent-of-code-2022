module Advent12 exposing (..)

-- import Data.Advent12Data exposing (testInput)

import Array exposing (Array)
import Char exposing (toCode)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Tree exposing (tree)
import Tree.Zipper exposing (Zipper, append)


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


nodeTraversable : Cell -> Cell -> Bool
nodeTraversable currentCell nextCell =
    nextCell.elevation <= (currentCell.elevation + 1) && nextCell.elevation >= (currentCell.elevation - 1)


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
    String.fromInt cell.x ++ "-" ++ String.fromInt cell.y


getNodesIfTravesable : Cell -> Array (Array Cell) -> List Cell
getNodesIfTravesable cell twoDMap =
    let
        up =
            getNode cell.x (cell.y - 1) twoDMap
                |> Maybe.andThen
                    (\newCell ->
                        if nodeTraversable cell newCell then
                            Just newCell

                        else
                            Nothing
                    )

        down =
            getNode cell.x (cell.y + 1) twoDMap
                |> Maybe.andThen
                    (\newCell ->
                        if nodeTraversable cell newCell then
                            Just newCell

                        else
                            Nothing
                    )

        left =
            getNode (cell.x - 1) cell.y twoDMap
                |> Maybe.andThen
                    (\newCell ->
                        if nodeTraversable cell newCell then
                            Just newCell

                        else
                            Nothing
                    )

        right =
            getNode (cell.x + 1) cell.y twoDMap
                |> Maybe.andThen
                    (\newCell ->
                        if nodeTraversable cell newCell then
                            Just newCell

                        else
                            Nothing
                    )
    in
    [ up, down, left, right ]
        |> List.filterMap identity


twoDArrayToGraph : Array (Array Cell) -> Graph
twoDArrayToGraph arr =
    arr
        |> Array.map Array.toList
        |> Array.toList
        |> List.concat
        |> List.map
            (\cell ->
                let
                    neighbours =
                        getNodesIfTravesable cell arr
                            |> List.map convertCellToKey
                in
                ( convertCellToKey cell
                , { key = convertCellToKey cell
                  , neighbours = neighbours
                  , destination = cell.cellType
                  }
                )
            )
        |> Dict.fromList


findStart : Graph -> Maybe GraphNode
findStart graph =
    graph
        |> Dict.toList
        |> List.filter (\item -> Tuple.second item |> (\x -> x.destination == Start))
        |> (\list ->
                case list of
                    [] ->
                        Nothing

                    x :: xs ->
                        case xs of
                            [] ->
                                Just (Tuple.second x)

                            _ :: _ ->
                                Nothing
           )


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


part1Solution : String -> Int
part1Solution input =
    let
        graph =
            input
                |> prepareInput
                |> twoDArrayToGraph
    in
    graph
        |> findStart
        |> Debug.log "start"
        |> Maybe.map (\start -> start.neighbours ++ [ start.key ])
        |> Maybe.withDefault []
        |> countNodesToEnd 0 graph


main : Html msg
main =
    view
