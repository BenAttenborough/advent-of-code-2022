module Advent12 exposing (..)

-- import Tree exposing (tree)
-- import Tree.Zipper exposing (Zipper, append)

import AlternativeSolutions.DirectoryParser exposing (Msg)
import Array exposing (Array)
import Char exposing (toCode)
import Data.Advent12Data exposing (brokenInput, testInput)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra as List
import Set exposing (Set)
import Tuple3
import Utilities.Utilities exposing (array2dToDict2d, uniqueItemFrom2DArray)


type alias Cell =
    ( Int, Int, Int )


type Msg
    = Advance


type alias Model =
    Dict ( Int, Int ) Int



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


findStart : Array (Array ( Int, Int, Char )) -> Maybe Cell
findStart charArray =
    charArray
        |> uniqueItemFrom2DArray (\item -> Tuple3.third item == startChar)
        |> Maybe.map (\( intX, intY, _ ) -> ( intX, intY, startElevation ))


findEnd : Array (Array ( Int, Int, Char )) -> Maybe Cell
findEnd charArray =
    charArray
        |> uniqueItemFrom2DArray (\item -> Tuple3.third item == endChar)
        |> Maybe.map (\( intX, intY, _ ) -> ( intX, intY, endElevation ))


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



-- prepareInput : String -> Array (Array Cell)
-- prepareInput =
--     String.lines
--         >> List.map
--             (String.toList
--                 >> List.map
--                     (\char ->
--                         ( 0, 0, charToCode char )
--                     )
--                 >> Array.fromList
--                 >> Array.indexedMap (\index ( _, y, z ) -> ( index, y, z ))
--             )
--         >> Array.fromList
--         >> Array.indexedMap (\index arr -> Array.map (\( x, _, z ) -> ( x, index, z )) arr)
-- VIEW helpers


printCharRow : Array ( Int, Int, Char ) -> Html msg
printCharRow row =
    Array.toList row
        -- |> List.map (\item -> div [] [ Html.text (Tuple3.third item |> String.fromChar) ])
        |> List.map Tuple3.third
        |> String.fromList
        |> (\str -> div [] [ Html.text str ])


printCharGrid : Array (Array ( Int, Int, Char )) -> List (Html msg)
printCharGrid grid =
    Array.toList grid
        |> List.map (\row -> printCharRow row)


checkInput : Array (Array ( Int, Int, Char )) -> Result String (Array (Array ( Int, Int, Char )))
checkInput input =
    case findStart input of
        Nothing ->
            Err "No start point!"

        Just _ ->
            case findEnd input of
                Nothing ->
                    Err "No end point!"

                Just _ ->
                    Ok input


view : Cell -> Html msg
view cell =
    let
        charArray =
            inputToCharArray testInput
    in
    div []
        [ Html.text (Debug.toString cell)
        ]


puzzleView : String -> Html msg
puzzleView input =
    let
        charArray : Array (Array ( Int, Int, Char ))
        charArray =
            inputToCharArray input
    in
    case checkInput charArray of
        Ok atlas ->
            div []
                (printCharGrid atlas)

        Err err ->
            div []
                [ Html.text err ]


update : Msg -> model -> model
update msg model =
    case msg of
        Advance ->
            model


main : Html msg
main =
    div []
        [ div [ class "panel" ]
            [ Html.button []
                [ Html.text "Go" ]
            ]
        , div [ class "panel" ]
            [ Html.text "Puzzle"
            , puzzleView brokenInput
            ]
        ]
