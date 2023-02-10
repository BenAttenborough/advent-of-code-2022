module Advent9Visualise exposing (..)

import Array exposing (Array)
import Html exposing (Html, p, text)
import Html.Attributes exposing (style)
import List exposing (head)
import Tree.Diff exposing (Tail(..))


type Cell
    = Empty
    | Head
    | Body
    | Tail


type alias Playfield =
    Array (Array Cell)


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


main : Html msg
main =
    makePlayfield 10 10 Array.empty
        |> updatePlayfield 2 2 Head
        |> playfieldToText
        |> List.map text
        |> List.map (\s -> Html.p [ style "margin" "0" ] [ s ])
        |> Html.div []
