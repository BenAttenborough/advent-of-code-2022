module Advent12 exposing (..)

import AlternativeSolutions.DirectoryParser exposing (Msg)
import Array exposing (Array)
import Browser
import Char exposing (fromCode, toCode)
import Data.Advent12Data exposing (testInput)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra as List
import Tuple3
import Utilities.Utilities exposing (array2dToDict, uniqueItemFrom2DArray)


type alias Coordinates =
    ( Int, Int )


type alias Cell =
    ( Int, Int, Int )


type alias Atlas =
    Array (Array Cell)


type alias UnvisitedCells =
    Dict ( Int, Int ) Int


type Msg
    = Advance


type alias Model =
    { currentCells : List Cell

    -- , visitedCells : List Coordinates
    , unvisitedCells : UnvisitedCells
    , steps : Int
    }



-- Initial model


initialModel : Model
initialModel =
    { currentCells = initCurrentCells

    -- , visitedCells = initVisitedCells
    , unvisitedCells = initUnvisitedCells
    , steps = 0
    }



-- Constants, can be adjusted for different problems


puzzleInput : String
puzzleInput =
    testInput


puzzleAtlas : Atlas
puzzleAtlas =
    inputToAtlas puzzleInput


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


codeToString : Int -> String
codeToString code =
    code
        + aCode
        |> fromCode
        |> String.fromChar



-- Cell helpers


coordinateX : Coordinates -> Int
coordinateX =
    Tuple.first


coordinateY : Coordinates -> Int
coordinateY =
    Tuple.second


inputToCharArray : String -> Array (Array ( Int, Int, Char ))
inputToCharArray input =
    input
        |> String.lines
        |> List.map
            (String.toList
                >> List.map
                    (\char ->
                        ( 0, 0, char )
                    )
                >> Array.fromList
                >> Array.indexedMap (\index ( _, y, z ) -> ( index, y, z ))
            )
        |> Array.fromList
        |> Array.indexedMap (\index arr -> Array.map (\( x, _, z ) -> ( x, index, z )) arr)


findStart : String -> Maybe Cell
findStart input =
    input
        |> inputToCharArray
        |> uniqueItemFrom2DArray (\item -> Tuple3.third item == startChar)
        |> Maybe.map (\item -> ( Tuple3.first item, Tuple3.second item, charToCode (Tuple3.third item) ))


findEnd : String -> Maybe Cell
findEnd input =
    input
        |> inputToCharArray
        |> uniqueItemFrom2DArray (\item -> Tuple3.third item == endChar)
        |> Maybe.map (\item -> ( Tuple3.first item, Tuple3.second item, charToCode (Tuple3.third item) ))


inputToAtlas : String -> Atlas
inputToAtlas =
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


array2dMap : (a -> b) -> Array (Array a) -> Array (Array b)
array2dMap function array =
    array |> Array.map (Array.map function)


array2dGetNode : Int -> Int -> Array (Array a) -> Maybe a
array2dGetNode x y twoDMap =
    twoDMap
        |> Array.get y
        |> Maybe.andThen (Array.get x)


nodeTraversable : Cell -> Cell -> Maybe Cell
nodeTraversable currentCell comparableCell =
    if Tuple3.third comparableCell <= (Tuple3.third comparableCell + 1) && Tuple3.third currentCell >= (Tuple3.third comparableCell - 1) then
        Just comparableCell

    else
        Nothing


getAvailableNeighbours : Cell -> Atlas -> List Cell
getAvailableNeighbours currentCell atlas =
    let
        up =
            array2dGetNode (Tuple3.first currentCell) (Tuple3.second currentCell - 1) atlas
                |> Maybe.andThen (\comparableCell -> nodeTraversable currentCell comparableCell)

        down =
            array2dGetNode (Tuple3.first currentCell) (Tuple3.second currentCell + 1) atlas
                |> Maybe.andThen (\comparableCell -> nodeTraversable currentCell comparableCell)

        left =
            array2dGetNode (Tuple3.first currentCell - 1) (Tuple3.second currentCell) atlas
                |> Maybe.andThen (\comparableCell -> nodeTraversable currentCell comparableCell)

        right =
            array2dGetNode (Tuple3.first currentCell + 1) (Tuple3.second currentCell) atlas
                |> Maybe.andThen (\comparableCell -> nodeTraversable currentCell comparableCell)
    in
    [ up, down, left, right ]
        |> List.filterMap identity



-- VIEW helpers


printCharRow : Array Cell -> Cell -> Cell -> UnvisitedCells -> Html msg
printCharRow row start end unvisited =
    Array.toList row
        |> List.map
            (\item ->
                let
                    x =
                        Tuple3.first item

                    y =
                        Tuple3.second item

                    cellStyle =
                        if Tuple3.first start == x && Tuple3.second start == y then
                            "red"

                        else if Tuple3.first end == x && Tuple3.second end == y then
                            "cyan"

                        else if Dict.member ( x, y ) unvisited then
                            "greenyellow"

                        else
                            "gray"
                in
                span [ style "color" cellStyle ] [ Html.text (codeToString (Tuple3.third item)) ]
            )
        |> (\list ->
                div []
                    list
           )


printCharGrid : Atlas -> Cell -> Cell -> UnvisitedCells -> List (Html msg)
printCharGrid grid start end unvisited =
    Array.toList grid
        |> List.map (\row -> printCharRow row start end unvisited)


findStartAndEnd : String -> Result String ( Cell, Cell )
findStartAndEnd input =
    case findStart input of
        Nothing ->
            Err "No start point!"

        Just start ->
            case findEnd input of
                Nothing ->
                    Err "No end point!"

                Just end ->
                    Ok ( start, end )


view : Model -> Html Msg
view model =
    div
        []
        [ div [ class "panel" ]
            [ Html.button [ onClick Advance ]
                [ Html.text "Go" ]
            ]
        , div [ class "panel" ]
            [ Html.text "Puzzle"
            , puzzleView testInput model
            ]
        ]


initVisitedCells : List Coordinates
initVisitedCells =
    []


initUnvisitedCells : UnvisitedCells
initUnvisitedCells =
    puzzleAtlas
        |> array2dMap Tuple3.third
        |> array2dToDict
        |> Dict.filter
            (\comparable _ ->
                not (List.member comparable initVisitedCells)
            )


initCurrentCells : List Cell
initCurrentCells =
    case findStart puzzleInput of
        Just start ->
            [ start ]

        Nothing ->
            []


puzzleView : String -> Model -> Html msg
puzzleView input model =
    case findStartAndEnd input of
        Ok ( start, end ) ->
            div []
                (printCharGrid puzzleAtlas start end model.unvisitedCells
                    ++ [ div [ style "margin-top" "1rem" ]
                            [ Html.text "visitedCells cells"

                            -- , Html.text (Debug.toString model.visitedCells)
                            , div [ style "margin-top" "1rem" ]
                                [ Html.text "Unvisited cells"
                                , Html.text (Debug.toString model.unvisitedCells)
                                ]
                            ]
                       ]
                )

        Err err ->
            div []
                [ Html.text err ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        Advance ->
            let
                neighbouringCells =
                    List.map (\cell -> getAvailableNeighbours cell puzzleAtlas) model.currentCells
                        |> List.concat
                        -- |> List.filter (\item -> not (List.member item model.visitedCells))
                        |> List.unique
            in
            { model | currentCells = neighbouringCells }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
