module AlternativeSolutions.DirectoryParser exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Decode exposing (Decoder, string)
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
    = OnChange String
    | OnKeyDown Int


view : Model -> Html Msg
view { command, directoryTree } =
    Html.div []
        [ div []
            [ directoryTree |> toHtml ]
        , input
            [ placeholder "Type your command"
            , value command
            , onInput OnChange
            , onKeyDown OnKeyDown
            ]
            []
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnChange command ->
            { model | command = command }

        OnKeyDown key ->
            if key == 13 then
                { model | directoryTree = singleton (Directory "root" [ File model.command 100 ]) }

            else
                model


onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown tagger =
    on "keydown" (Decode.map tagger keyCode)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
