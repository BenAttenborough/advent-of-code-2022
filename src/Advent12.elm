module Advent12 exposing (..)

-- import Tree exposing (tree)
-- import Tree.Zipper exposing (Zipper, append)
-- import Set exposing (Set)
-- import Advent9 exposing (coordinatesX, coordinatesY)

import AlternativeSolutions.DirectoryParser exposing (Msg)
import Array exposing (Array)
import Char exposing (fromCode, toCode)
import Data.Advent12Data exposing (brokenInput)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra as List
import Tuple3
import Utilities.Utilities exposing (array2dToDict2d, uniqueItemFrom2DArray)


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
                >> Array.indexedMap (\index ( x, _, z ) -> ( x, index, z ))
            )
        |> Array.fromList
        |> Array.indexedMap (\index arr -> Array.map (\( _, y, z ) -> ( index, y, z )) arr)


findStart : String -> Maybe Cell
findStart input =
    input
        |> inputToCharArray
        |> uniqueItemFrom2DArray (\item -> Tuple3.third item == startChar)
        |> Maybe.map (\item -> ( Tuple3.first item, Tuple3.second item, toCode (Tuple3.third item) ))


findEnd : String -> Maybe Cell
findEnd input =
    input
        |> inputToCharArray
        |> uniqueItemFrom2DArray (\item -> Tuple3.third item == endChar)
        |> Maybe.map (\item -> ( Tuple3.first item, Tuple3.second item, toCode (Tuple3.third item) ))


inputToAtlas : String -> Atlas
inputToAtlas =
    String.lines
        >> List.map
            (String.toList
                >> List.map
                    (\char ->
                        ( 0, 0, toCode char )
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
                    _ =
                        Debug.log "Dict" unvisited

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
                span [ style "color" cellStyle ] [ Html.text ((String.fromChar << fromCode << Tuple3.third) item) ]
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


view : Coordinates -> Html msg
view cell =
    div []
        [ Html.text (Debug.toString cell)
        ]


puzzleView : String -> Html msg
puzzleView input =
    let
        atlas : Atlas
        atlas =
            inputToAtlas input
    in
    case findStartAndEnd input of
        Ok ( start, end ) ->
            let
                visitedCells : List Coordinates
                visitedCells =
                    getAvailableNeighbours start atlas
                        |> List.map (\item -> ( Tuple3.first item, Tuple3.second item ))

                unvisitedCells : UnvisitedCells
                unvisitedCells =
                    atlas
                        |> array2dMap Tuple3.third
                        |> array2dToDict2d
                        |> Dict.filter
                            (\comparable _ ->
                                not (List.member comparable visitedCells)
                            )
            in
            div []
                (printCharGrid atlas start end unvisitedCells
                    ++ [ div []
                            [ Html.text "Unvisited cells"
                            , Html.text (Debug.toString visitedCells)
                            ]
                       ]
                )

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
