module Advent12 exposing (..)

import Array exposing (Array)
import Char exposing (toCode)
import Data.Advent12Data exposing (testInput)
import Html exposing (..)
import Html.Attributes exposing (..)
import Parser exposing (end)


type Tile
    = Start
    | Journey
    | End


type alias Cell =
    { elevation : Int
    , cellType : Tile
    }


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
