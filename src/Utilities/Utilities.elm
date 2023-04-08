module Utilities.Utilities exposing (array2dToDict2d, build2DArray, getElementFrom2DArray, linesDebugToHtml, linesToHtml, partitioner, uniqueItemFrom2DArray)

import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html, p, text)
import Html.Attributes exposing (class, list)


linesDebugToHtml : List a -> Html msg
linesDebugToHtml list =
    list
        |> List.map
            (\line ->
                p [ class "command-line" ] [ text (Debug.toString line) ]
            )
        |> (\l ->
                Html.div []
                    l
           )


linesToHtml : List String -> Html msg
linesToHtml list =
    list
        |> List.map
            (\line ->
                p [ class "command-line" ] [ text line ]
            )
        |> (\l ->
                Html.div []
                    l
           )


partitioner : Int -> List (List a) -> List a -> List (List a)
partitioner size container list =
    if size < 1 then
        [ list ]

    else if List.length list <= size then
        list
            :: container
            |> List.reverse

    else
        let
            newContainer =
                List.take size list :: container
        in
        partitioner size newContainer (List.drop size list)


getElementFrom2DArray : Int -> Int -> Array (Array a) -> Maybe a
getElementFrom2DArray x y twoDMap =
    twoDMap
        |> Array.get y
        |> Maybe.andThen (Array.get x)


build2DArray : List (List a) -> Array (Array a)
build2DArray list =
    list
        |> List.map Array.fromList
        |> Array.fromList


uniqueItemFrom2DArray : (a -> Bool) -> Array (Array a) -> Maybe a
uniqueItemFrom2DArray test atlas =
    atlas
        |> Array.map
            (\arr ->
                Array.filter test arr
                    |> Array.get 0
            )
        |> Array.toList
        |> List.filterMap identity
        |> (\list ->
                if List.length list == 1 then
                    List.head list

                else
                    Nothing
           )


array2dToDict2d : Array (Array a) -> Dict ( Int, Int ) a
array2dToDict2d x =
    x
        |> Array.map
            Array.toIndexedList
        |> Array.toIndexedList
        |> List.map
            (\tuple ->
                let
                    xIndex =
                        Tuple.first tuple

                    tupleValue =
                        Tuple.second tuple

                    value =
                        tupleValue
                            |> List.map
                                (\( yIndex, value_ ) ->
                                    ( ( xIndex, yIndex ), value_ )
                                )
                in
                ( xIndex, value )
            )
        |> List.map
            Tuple.second
        |> List.concat
        |> Dict.fromList
