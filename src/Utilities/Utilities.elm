module Utilities.Utilities exposing (linesDebugToHtml, linesToHtml, partitioner)

import Html exposing (Html, p, text)
import Html.Attributes exposing (class)


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
