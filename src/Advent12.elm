module Advent12 exposing (..)

-- import Data.Advent12Data exposing (testInput)

import Array exposing (Array)
import Char exposing (toCode)
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



-- Use zipper to append child and zip to it
-- Maybe for each node we add all valid child nodes
-- then recursively do the same for each child
-- this is for a two dimensional array
-- so we need a function that gets up, right, down and left nodes
-- may need to track which nodes have already been visited


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
    }


type alias Graph =
    List GraphNode


convertCellToKey : Cell -> String
convertCellToKey cell =
    String.fromInt cell.x ++ "-" ++ String.fromInt cell.y


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
                        getNodes cell.x cell.y arr
                            |> List.map convertCellToKey
                in
                { key = convertCellToKey cell
                , neighbours = neighbours
                }
            )



-- |> Array.map (\a -> Array.map (\cell -> {key = "a", neighbours = []}))


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


main : Html msg
main =
    view
