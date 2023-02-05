module Advent9 exposing (..)

import Advent9Data exposing (testInput)
import Html exposing (Html, p, text)
import Json.Decode exposing (list, oneOf)
import Maybe.Extra exposing (isJust)
import Parser exposing (..)


type Command
    = Up
    | Down
    | Left
    | Right


type alias Rope =
    { tail : ( Int, Int )
    , headRel : ( Int, Int )
    , visited : List ( Int, Int )
    }


initialRopeState : Rope
initialRopeState =
    { tail = ( 0, 0 )
    , headRel = ( 0, 0 )
    , visited = [ ( 0, 0 ) ]
    }


coordinatesX : ( Int, Int ) -> Int
coordinatesX coords =
    Tuple.first coords


coordinatesY : ( Int, Int ) -> Int
coordinatesY coords =
    Tuple.second coords


applyCommandsToRopeState : Command -> Rope -> Rope
applyCommandsToRopeState command initialState =
    case command of
        Up ->
            let
                relHeadPos =
                    ( coordinatesX initialState.headRel
                    , min 1 (coordinatesY initialState.headRel + 1)
                    )

                tailPos =
                    if (coordinatesY initialState.headRel + 1) > 1 then
                        ( coordinatesX initialState.headRel
                        , coordinatesY initialState.tail + 1
                        )

                    else
                        initialState.tail

                visitedLocations =
                    tailPos :: initialState.visited
            in
            { tail = tailPos
            , headRel = relHeadPos
            , visited = visitedLocations
            }

        Down ->
            initialState

        Left ->
            initialState

        Right ->
            initialState


main : Html msg
main =
    testInput
        |> parseCommandsFromInput
        |> List.foldl applyCommandsToRopeState
            initialRopeState
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
