module Advent12 exposing (..)

import Array exposing (Array)
import Char exposing (toCode)
import Data.Advent12Data exposing (testInput)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode exposing (array)
import Parser exposing (end)
import Tree exposing (Tree, tree)
import Tree.Zipper exposing (Zipper, append)


type Tile
    = Start
    | Journey
    | End


type alias Cell =
    { elevation : Int
    , cellType : Tile
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


charToCode : Char -> Cell
charToCode =
    toCode
        >> (\code ->
                if code == startCharCode then
                    Cell startElevation Start

                else if code == endCharCode then
                    Cell endElevation End

                else
                    Cell (code - aCode) Journey
           )


prepareInput : String -> Array (Array Cell)
prepareInput =
    String.lines
        >> List.map
            (String.toList
                >> List.map charToCode
                >> Array.fromList
            )
        >> Array.fromList


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
