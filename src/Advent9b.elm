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
    Tuple.first coords


makeRope : Int -> Rope -> Rope
makeRope size rope =
    if size < 1 then
        rope

    else
        makeRope (size - 1) (( 0, 0 ) :: rope)


moveKnot : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
moveKnot ( orX, orY ) ( relX, relY ) =
    ( orX + relX, orY + relY )



-- Only moves knot in relative position from a range of -1 to 1 in bopth axis
-- This is relative to the next knot. If movement would cause the knot to move
-- beyond the -1 to 1 range this is added to the transform - the transform is then
-- applied to the next knot and so on


moveKnotRelative : ( Int, Int ) -> ( Int, Int ) -> ( ( Int, Int ), ( Int, Int ) )
moveKnotRelative ( orX, orY ) ( relX, relY ) =
    ( ( relX, relY ), ( orX, orY ) )



-- knotTransform : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
-- knotTransform ( orX, orY ) ( relX, relY ) =
--     if (orX + relX) > 1 || (orX + relX) < 0 then
--         if (orX + relX) > 1 then
--             ( 1, relY )
--         else
--             ( -1, relY )
--     else if (orY + relY) > 1 || (orY + relY) < 0 then
--         if (orY + relY) > 1 then
--             ( relX, 1 )
--         else
--             ( relX, -1 )
--     else
--         ( relX, relY )
-- moveRope : Rope -> Command -> ( Int, Int ) -> Rope
-- moveRope rope command ( transX, transY ) =
--     rope
--         |> List.foldl
--             (\knot x ->
--                 case command of
--                     Up ->
--                         ( knot, ( 0, 1 ) )
--                     _ ->
--                         ( knot, ( 0, 0 ) )
--             )


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
