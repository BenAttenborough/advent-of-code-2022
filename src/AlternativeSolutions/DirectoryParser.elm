module AlternativeSolutions.DirectoryParser exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Tree.Zipper as Zipper
import Utilities.DirectoryTree exposing (..)


type alias Model =
    { command : String
    , directoryTree : Zipper.Zipper Directory
    }


initialModel : Model
initialModel =
    { command = ""
    , directoryTree = singleton (Directory "root" [])
    }


type Msg
    = SubmitCommand String
    | OnChange String


view : Model -> Html Msg
view { command, directoryTree } =
    Html.div []
        [ div []
            [ directoryTree |> toHtml
            ]
        , input [ placeholder "Type your command", value command, onInput OnChange ] []
        , button [ onClick (SubmitCommand command) ] [ text "Submit" ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        SubmitCommand command ->
            { model | directoryTree = singleton (Directory "root" [ File command 100 ]) }

        OnChange command ->
            { model | command = command }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
