module Advent12 exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view : a -> Html msg
view _ =
    div []
        [ pre [ style "white-space" "pre-line" ]
            [ text "TEST\n\n"
            ]
        ]


main : Html msg
main =
    view "dummy model"
