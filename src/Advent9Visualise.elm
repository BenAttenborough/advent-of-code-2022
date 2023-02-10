module Advent9Visualise exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (Html, p, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import List exposing (head)
import Tree.Diff exposing (Tail(..))


type Cell
    = Empty
    | Head
    | Body
    | Tail


type alias Playfield =
    Array (Array Cell)


type Command
    = Up
    | Down
    | Left
    | Right
    | Other


type alias Model =
    { playfield : Playfield }


type Msg
    = KeyPressed


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyPress (Decode.succeed KeyPressed)


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { playfield = makePlayfield 10 10 Array.empty }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPressed ->
            ( model, Cmd.none )


view : Model -> Html Msg
view { playfield } =
    playfield
        |> updatePlayfield 5 5 Head
        |> playfieldToText
        |> List.map text
        |> List.map (\s -> Html.p [ style "margin" "0" ] [ s ])
        |> Html.div []


initialRope : ( Int, Int )
initialRope =
    ( 5, 5 )


makeRow : Int -> List Cell -> Array Cell
makeRow width currentRow =
    if width < 1 then
        Array.fromList currentRow

    else
        makeRow (width - 1) (Empty :: currentRow)


makePlayfield : Int -> Int -> Playfield -> Playfield
makePlayfield width height currentPlayfield =
    if height < 1 then
        currentPlayfield

    else
        makePlayfield width (height - 1) (Array.push (makeRow width []) currentPlayfield)


cellToText : Cell -> Char
cellToText cell =
    case cell of
        Empty ->
            '.'

        Head ->
            'H'

        Body ->
            '*'

        Tail ->
            'T'


rowToText : Array Cell -> String
rowToText cells =
    cells
        |> Array.map cellToText
        |> Array.toList
        |> String.fromList


playfieldToText : Playfield -> List String
playfieldToText playfield =
    playfield
        |> Array.map rowToText
        |> Array.toList


updatePlayfield : Int -> Int -> Cell -> Playfield -> Playfield
updatePlayfield row col cell playfield =
    playfield
        |> Array.get row
        |> Maybe.map (Array.set col cell)
        |> Maybe.map (\replacementRow -> Array.set row replacementRow playfield)
        |> Maybe.withDefault playfield


keyDecoder : Decode.Decoder Command
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Command
toDirection string =
    case string of
        "ArrowUp" ->
            Up

        "ArrowDown" ->
            Down

        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        _ ->
            Other
