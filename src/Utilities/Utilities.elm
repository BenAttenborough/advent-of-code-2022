module Utilities.Utilities exposing (linesDebugToHtml, linesToHtml)

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
