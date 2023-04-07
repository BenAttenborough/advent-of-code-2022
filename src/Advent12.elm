module Advent12 exposing (..)

-- import Tree exposing (tree)
-- import Tree.Zipper exposing (Zipper, append)

import Advent9 exposing (coordinatesX, coordinatesY)
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


type alias Coordinates =
    ( Int, Int )


type alias VisualCell =
    ( Int, Int, Char )


type alias Atlas =
    Array (Array VisualCell)


type alias UnvisitedCells =
    Dict ( Int, Int ) Int


type Msg
    = Advance



-- type alias Model =
--     Dict ( Int, Int ) Int
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


coordinateX : Coordinates -> Int
coordinateX =
    Tuple.first


coordinateY : Coordinates -> Int
coordinateY =
    Tuple.second


findStart : Atlas -> Maybe Coordinates
findStart charArray =
    charArray
        |> uniqueItemFrom2DArray (\item -> Tuple3.third item == startChar)
        |> Maybe.map (\( intX, intY, _ ) -> ( intX, intY ))


findEnd : Atlas -> Maybe Coordinates
findEnd charArray =
    charArray
        |> uniqueItemFrom2DArray (\item -> Tuple3.third item == endChar)
        |> Maybe.map (\( intX, intY, _ ) -> ( intX, intY ))


inputToCharArray : String -> Atlas
inputToCharArray =
    String.lines
        >> List.map
            (String.toList
                >> List.map
                    (\char ->
                        ( 0, 0, char )
                    )
                >> Array.fromList
                >> Array.indexedMap (\index ( x, _, z ) -> ( x, index, z ))
            )
        >> Array.fromList
        >> Array.indexedMap (\index arr -> Array.map (\( _, y, z ) -> ( index, y, z )) arr)


array2dMap : (a -> b) -> Array (Array a) -> Array (Array b)
array2dMap function array =
    array |> Array.map (Array.map function)


array2dGetNode : Int -> Int -> Array (Array a) -> Maybe a
array2dGetNode x y twoDMap =
    twoDMap
        |> Array.get y
        |> Maybe.andThen (Array.get x)



-- getAvailableNeighbours : VisualCell -> Atlas -> String -> List String
-- getAvailableNeighbours cell atlas parentKey =
--     let
--         up =
--             array2dGetNode (coordinateX cell) (cell.y - 1) atlas
--                 |> Maybe.andThen (nodeTraversable cell)
--         down =
--             getNode cell.x (cell.y + 1) atlas
--                 |> Maybe.andThen (nodeTraversable cell)
--         left =
--             getNode (cell.x - 1) cell.y atlas
--                 |> Maybe.andThen (nodeTraversable cell)
--         right =
--             getNode (cell.x + 1) cell.y atlas
--                 |> Maybe.andThen (nodeTraversable cell)
--     in
--     [ up, down, left, right ]
--         |> List.filterMap identity
--         |> List.map convertCellToKey
--         |> List.filter (\item -> not (item == parentKey))
-- VIEW helpers


printCharRow : Array VisualCell -> Coordinates -> Coordinates -> UnvisitedCells -> Html msg
printCharRow row start end unvisited =
    Array.toList row
        |> List.map
            (\item ->
                let
                    _ =
                        Debug.log "Dict" unvisited

                    x =
                        Tuple3.first item

                    y =
                        Tuple3.second item

                    cellStyle =
                        if coordinatesX start == x && coordinatesY start == y then
                            "red"

                        else if coordinatesX end == x && coordinatesY end == y then
                            "cyan"

                        else if Dict.member ( x, y ) unvisited then
                            "greenyellow"

                        else
                            "gray"
                in
                span [ style "color" cellStyle ] [ Html.text ((String.fromChar << Tuple3.third) item) ]
            )
        |> (\list ->
                div []
                    list
           )


printCharGrid : Atlas -> Coordinates -> Coordinates -> UnvisitedCells -> List (Html msg)
printCharGrid grid start end unvisited =
    Array.toList grid
        |> List.map (\row -> printCharRow row start end unvisited)


checkInput : Atlas -> Result String ( Coordinates, Coordinates )
checkInput input =
    case findStart input of
        Nothing ->
            Err "No start point!"

        Just start ->
            case findEnd input of
                Nothing ->
                    Err "No end point!"

                Just end ->
                    Ok ( start, end )


view : Coordinates -> Html msg
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
        charArray : Atlas
        charArray =
            inputToCharArray input

        unvisitedCells : UnvisitedCells
        unvisitedCells =
            charArray
                |> array2dMap (Tuple3.third >> charToCode)
                |> array2dToDict2d

        -- |> Dict.remove ( 1, 1 )
    in
    case checkInput charArray of
        Ok ( start, end ) ->
            div []
                (printCharGrid charArray start end unvisitedCells)

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
