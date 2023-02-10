module Advent9Visualise exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown, onKeyPress)
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


type Msg
    = Up
    | Down
    | Left
    | Right
    | Other


type alias Model =
    { playfield : Playfield
    , head : ( Int, Int )
    }


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
    onKeyDown keyDecoder


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { playfield = makePlayfield 10 10 Array.empty
      , head = ( 5, 5 )
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Up ->
            ( { model | head = ( Tuple.first model.head - 1, Tuple.second model.head ) }
            , Cmd.none
            )

        Down ->
            ( { model | head = ( Tuple.first model.head + 1, Tuple.second model.head ) }
            , Cmd.none
            )

        Left ->
            ( { model | head = ( Tuple.first model.head, Tuple.second model.head - 1 ) }
            , Cmd.none
            )

        Right ->
            ( { model | head = ( Tuple.first model.head, Tuple.second model.head + 1 ) }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view { playfield, head } =
    playfield
        |> updatePlayfield (Tuple.first head) (Tuple.second head) Head
        |> playfieldToText
        |> List.map text
        |> List.map (\s -> Html.p [ style "margin" "0" ] [ s ])
        |> Html.div []


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


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Msg
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
