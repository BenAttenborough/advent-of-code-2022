module Advent9b exposing (..)

import Advent9Data exposing (realInput, testInput)
import Html exposing (Html, p, text)
import Json.Decode exposing (list, oneOf)
import Maybe.Extra exposing (isJust)
import Parser exposing (..)
import Set exposing (Set)


type Command
    = Up
    | Down
    | Left
    | Right


type alias Rope =
    List ( Int, Int )


ropeTail =
    ( 0, 0 )


getX : ( Int, Int ) -> Int
getX coords =
    Tuple.first coords


getY : ( Int, Int ) -> Int
getY coords =
    Tuple.second coords


makeRope : Int -> Rope -> Rope
makeRope size rope =
    if size < 1 then
        rope

    else
        makeRope (size - 1) (( 0, 0 ) :: rope)


commandToTransform : Command -> ( Int, Int )
commandToTransform command =
    case command of
        Up ->
            ( 0, 1 )

        Down ->
            ( 0, -1 )

        Right ->
            ( 1, 0 )

        Left ->
            ( -1, 0 )


moveKnot : Command -> ( Int, Int ) -> ( Int, Int )
moveKnot command ( x, y ) =
    let
        transform =
            commandToTransform command
    in
    ( x + getX transform, y + getY transform )


moveKnotRelativeToLast : Command -> ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
moveKnotRelativeToLast command ( lastX, lastY ) ( curX, curY ) =
    case command of
        Up ->
            if lastY - curY > 1 then
                ( lastX, curY + 1 )

            else
                ( curX, curY )

        Down ->
            if lastY + curY < -1 || lastY + curY > 1 then
                ( lastX, curY - 1 )

            else
                ( curX, curY )

        Right ->
            if lastX - curX > 1 then
                ( curX + 1, curY )

            else
                ( curX, curY )

        Left ->
            if lastX + curX < -1 || lastX + curX > 1 then
                ( curX - 1, lastY )

            else
                ( curX, curY )


main : Html msg
main =
    realInput
        |> parseCommandsFromInput
        -- |> applyCommandsToRope (makeRope 10 [])
        |> Debug.toString
        |> Html.text


parseCommandsFromInput : String -> List Command
parseCommandsFromInput input =
    input
        |> String.lines
        |> List.map (Parser.run commandParser)
        |> List.map Result.toMaybe
        |> List.filterMap identity
        |> List.concat


createNCommands : Int -> Command -> List Command -> List Command
createNCommands amount command list =
    if amount < 1 then
        list

    else
        createNCommands (amount - 1) command (command :: list)


commandParser : Parser (List Command)
commandParser =
    succeed identity
        |= Parser.oneOf
            [ succeed (\amount -> createNCommands amount Up [])
                |. keyword "U"
                |. spaces
                |= int
            , succeed (\amount -> createNCommands amount Down [])
                |. keyword "D"
                |. spaces
                |= int
            , succeed (\amount -> createNCommands amount Left [])
                |. keyword "L"
                |. spaces
                |= int
            , succeed (\amount -> createNCommands amount Right [])
                |. keyword "R"
                |. spaces
                |= int
            ]
