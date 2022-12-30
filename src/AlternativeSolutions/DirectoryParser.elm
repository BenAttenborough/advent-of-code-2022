module AlternativeSolutions.DirectoryParser exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Decode
import Parser exposing (..)
import Tree.Zipper as Zipper
import Utilities.DirectoryTree exposing (..)


type alias Model =
    { terminalInput : String
    , directoryTree : Zipper.Zipper Directory
    , terminalOutput : List String
    }


initialModel : Model
initialModel =
    { terminalInput = ""
    , directoryTree = singleton (Directory "root" [])
    , terminalOutput = []
    }


type Msg
    = OnChange String
    | OnKeyDown Int


view : Model -> Html Msg
view { terminalInput, directoryTree, terminalOutput } =
    Html.div []
        [ div []
            [ directoryTree |> toHtml ]
        , div [ class "terminalOutput" ]
            (List.map (\line -> div [] [ text ("> " ++ line) ]) terminalOutput)
        , input
            [ placeholder "Type your command"
            , value terminalInput
            , onInput OnChange
            , onKeyDown OnKeyDown
            ]
            []
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnChange terminalInput ->
            { model | terminalInput = terminalInput }

        OnKeyDown key ->
            if key == 13 then
                let
                    parserResult =
                        Parser.run commandParser model.terminalInput
                in
                case parserResult of
                    Ok command ->
                        case command of
                            CD directoryName ->
                                case changeDirectoryCommand directoryName model.directoryTree of
                                    Ok value ->
                                        { model
                                            | directoryTree = value
                                            , terminalOutput =
                                                List.append model.terminalOutput [ "Change directory: " ++ directoryName ]
                                            , terminalInput = ""
                                        }

                                    Err err ->
                                        { model
                                            | terminalOutput =
                                                List.append model.terminalOutput [ err ]
                                            , terminalInput = ""
                                        }

                            LS ->
                                { model
                                    | terminalOutput =
                                        List.append model.terminalOutput [ "List" ]
                                    , terminalInput = ""
                                }

                            MakeDir name ->
                                case addFolderCommand (Directory name []) model.directoryTree of
                                    Ok val ->
                                        { model
                                            | directoryTree = val
                                            , terminalOutput =
                                                List.append model.terminalOutput [ "Made directory: " ++ name ]
                                            , terminalInput = ""
                                        }

                                    Err err ->
                                        { model
                                            | terminalOutput =
                                                List.append model.terminalOutput [ err ]
                                            , terminalInput = ""
                                        }

                            Touch fileName fileSize ->
                                { model
                                    | directoryTree = addFile (File fileName fileSize) model.directoryTree
                                    , terminalOutput =
                                        List.append model.terminalOutput [ "Created file" ]
                                    , terminalInput = ""
                                }

                    Err error ->
                        { model
                            | terminalOutput =
                                List.append model.terminalOutput [ "Error: " ++ Parser.deadEndsToString error ]
                            , terminalInput = ""
                        }

            else
                model


onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown tagger =
    on "keydown" (Decode.map tagger keyCode)


type Command
    = CD String
    | LS
    | MakeDir String
    | Touch String Int


word : Parser String
word =
    getChompedString <|
        succeed ()
            |. chompIf Char.isAlphaNum
            |. chompWhile Char.isAlphaNum


commandParser : Parser Command
commandParser =
    succeed identity
        |= oneOf
            [ succeed CD
                |. keyword "cd"
                |. spaces
                |= word
            , succeed LS
                |. keyword "ls"
            , succeed MakeDir
                |. keyword "mkdir"
                |. spaces
                |= word
            , succeed Touch
                |. keyword "touch"
                |. spaces
                |= word
                |. spaces
                |= int
            ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
