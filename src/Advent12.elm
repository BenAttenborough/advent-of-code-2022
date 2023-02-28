module Advent12 exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view model =
    div []
        [ pre [ style "white-space" "pre-line" ]
            [ text "TEST\n\n"
            ]
        ]


main =
    view "dummy model"