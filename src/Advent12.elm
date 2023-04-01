module Advent12 exposing (..)

-- import Dict exposing (Dict)
-- import Tree exposing (tree)
-- import Tree.Zipper exposing (Zipper, append)
-- import Utilities.Utilities exposing (uniqueItemFrom2DArray)

import Array exposing (Array)
import Char exposing (toCode)
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra as List
import Set exposing (Set)
import Tuple3


type alias Cell =
    ( Int, Int, Int )



-- Constants, can be adjusted for different problems


aCode : number
aCode =
    97


startChar : Char
startChar =
    'S'


endChar : Char
endChar =
    'E'


startElevation : Int
startElevation =
    0


endElevation : Int
endElevation =
    25



-- Converting special characters


startCharCode : Int
startCharCode =
    toCode startChar


endCharCode : Int
endCharCode =
    toCode endChar


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



-- Cell helpers


cellX : Cell -> Int
cellX =
    Tuple3.first


cellY : Cell -> Int
cellY =
    Tuple3.second


cellZ : Cell -> Int
cellZ =
    Tuple3.third



-- findStart : String -> Maybe Cell
-- findStart stringInput =
--     stringInput
--         |> inputToCharArray
--         |> uniqueItemFrom2DArray (\item -> Tuple3.third item == 'S')
-- findEnd : String -> Maybe Cell
-- findEnd stringInput =
--     stringInput
--         |> inputToCharArray
--         |> uniqueItemFrom2DArray (\item -> Tuple3.third item == 'E')


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


main : Html msg
main =
    view
