module Advent9 exposing (..)

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
    { tail : ( Int, Int )
    , headRel : ( Int, Int )
    , visited : Set ( Int, Int )
    }


initialRopeState : Rope
initialRopeState =
    { tail = ( 0, 0 )
    , headRel = ( 0, 0 )
    , visited = Set.singleton ( 0, 0 )
    }


coordinatesX : ( Int, Int ) -> Int
coordinatesX coords =
    Tuple.first coords


coordinatesY : ( Int, Int ) -> Int
coordinatesY coords =
    Tuple.second coords


applyCommandsToRopeState : Command -> Rope -> Rope
applyCommandsToRopeState command initialState =
    -- let
    --     x =
    --         Debug.log "head rel position:" initialState.headRel
    --     _ =
    --         Debug.log "Tail position:" initialState.tail
    --     y =
    --         Debug.log "=" "==============="
    -- in
    case command of
        Up ->
            let
                relHeadPos =
                    ( if (coordinatesY initialState.headRel + 1) > 1 then
                        0

                      else
                        coordinatesX initialState.headRel
                    , min 1 (coordinatesY initialState.headRel + 1)
                    )

                tailPos =
                    if (coordinatesY initialState.headRel + 1) > 1 then
                        ( coordinatesX initialState.tail + coordinatesX initialState.headRel
                        , coordinatesY initialState.tail + 1
                        )

                    else
                        initialState.tail

                visitedLocations =
                    Set.insert tailPos initialState.visited
            in
            { tail = tailPos
            , headRel = relHeadPos
            , visited = visitedLocations
            }

        Down ->
            let
                relHeadPos =
                    ( if (coordinatesY initialState.headRel - 1) < -1 then
                        0

                      else
                        coordinatesX initialState.headRel
                    , max -1 (coordinatesY initialState.headRel - 1)
                    )

                tailPos =
                    if (coordinatesY initialState.headRel - 1) < -1 then
                        ( coordinatesX initialState.tail + coordinatesX initialState.headRel
                        , coordinatesY initialState.tail - 1
                        )

                    else
                        initialState.tail

                visitedLocations =
                    Set.insert tailPos initialState.visited
            in
            { tail = tailPos
            , headRel = relHeadPos
            , visited = visitedLocations
            }

        Left ->
            let
                relHeadPos =
                    ( max -1 (coordinatesX initialState.headRel - 1)
                    , if (coordinatesX initialState.headRel - 1) < -1 then
                        0

                      else
                        coordinatesY initialState.headRel
                    )

                tailPos =
                    if (coordinatesX initialState.headRel - 1) < -1 then
                        ( coordinatesX initialState.tail - 1
                        , coordinatesY initialState.tail + coordinatesY initialState.headRel
                        )

                    else
                        initialState.tail

                visitedLocations =
                    Set.insert tailPos initialState.visited
            in
            { tail = tailPos
            , headRel = relHeadPos
            , visited = visitedLocations
            }

        Right ->
            let
                relHeadPos =
                    ( min 1 (coordinatesX initialState.headRel + 1)
                    , if (coordinatesX initialState.headRel + 1) > 1 then
                        0

                      else
                        coordinatesY initialState.headRel
                    )

                tailPos =
                    if (coordinatesX initialState.headRel + 1) > 1 then
                        ( coordinatesX initialState.tail + 1
                        , coordinatesY initialState.tail + coordinatesY initialState.headRel
                        )

                    else
                        initialState.tail

                visitedLocations =
                    Set.insert tailPos initialState.visited
            in
            { tail = tailPos
            , headRel = relHeadPos
            , visited = visitedLocations
            }


main : Html msg
main =
    realInput
        |> parseCommandsFromInput
        |> List.foldl applyCommandsToRopeState
            initialRopeState
        |> .visited
        |> Set.toList
        |> List.length
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
