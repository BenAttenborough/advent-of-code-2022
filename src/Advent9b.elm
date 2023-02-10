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


applyCommandsToKnot : Command -> ( Int, Int ) -> ( Int, Int )
applyCommandsToKnot command ( x, y ) =
    case command of
        Up ->
            ( x, y + 1 )

        Right ->
            ( x + 1, y )

        Down ->
            ( x, y - 1 )

        Left ->
            ( x - 1, y )


applyCommandToRope : Command -> Rope -> Rope
applyCommandToRope command rope =
    case rope of
        [] ->
            rope

        head :: tail ->
            let
                updatedHead =
                    applyCommandsToKnot command head
            in
            case tail of
                [] ->
                    [ updatedHead ]

                tailHead :: _ ->
                    let
                        headX =
                            Tuple.first updatedHead

                        headY =
                            Tuple.second updatedHead

                        tailHeadX =
                            Tuple.first tailHead

                        tailHeadY =
                            Tuple.second tailHead
                    in
                    case command of
                        Up ->
                            if (headY - tailHeadY) > 1 then
                                updatedHead :: applyCommandToRope command tail

                            else
                                updatedHead :: tail

                        Right ->
                            if (headX - tailHeadX) > 1 then
                                updatedHead :: applyCommandToRope command tail

                            else
                                updatedHead :: tail

                        Down ->
                            if (tailHeadY - headY) > 1 then
                                updatedHead :: applyCommandToRope command tail

                            else
                                updatedHead :: tail

                        Left ->
                            if (tailHeadX - headX) > 1 then
                                updatedHead :: applyCommandToRope command tail

                            else
                                updatedHead :: tail


applyCommandToRopeRecordTail : Command -> ( Rope, Set ( Int, Int ) ) -> ( Rope, Set ( Int, Int ) )
applyCommandToRopeRecordTail command ( rope, visited ) =
    case rope of
        [] ->
            ( rope, visited )

        head :: tail ->
            let
                updatedHead =
                    applyCommandsToKnot command head
            in
            case tail of
                [] ->
                    ( [ updatedHead ], Set.insert updatedHead visited )

                tailHead :: _ ->
                    let
                        headX =
                            Tuple.first updatedHead

                        headY =
                            Tuple.second updatedHead

                        tailHeadX =
                            Tuple.first tailHead

                        tailHeadY =
                            Tuple.second tailHead
                    in
                    case command of
                        Up ->
                            if (headY - tailHeadY) > 1 then
                                ( updatedHead :: applyCommandToRope command tail, visited )

                            else
                                ( updatedHead :: tail, visited )

                        Right ->
                            if (headX - tailHeadX) > 1 then
                                ( updatedHead :: applyCommandToRope command tail, visited )

                            else
                                ( updatedHead :: tail, visited )

                        Down ->
                            if (tailHeadY - headY) > 1 then
                                ( updatedHead :: applyCommandToRope command tail, visited )

                            else
                                ( updatedHead :: tail, visited )

                        Left ->
                            if (tailHeadX - headX) > 1 then
                                ( updatedHead :: applyCommandToRope command tail, visited )

                            else
                                ( updatedHead :: tail, visited )


applyCommandsToRope : Rope -> List Command -> Rope
applyCommandsToRope rope commands =
    List.foldl
        (\command knots ->
            applyCommandToRope command knots
        )
        rope
        commands


applyCommandsToRopeRecordTail : Rope -> List Command -> ( Rope, Set ( Int, Int ) )
applyCommandsToRopeRecordTail rope commands =
    List.foldl
        (\command knots ->
            applyCommandToRopeRecordTail command knots
        )
        ( rope, Set.fromList [ ( 0, 0 ) ] )
        commands


output : List Command -> ( Rope, Set ( Int, Int ) )
output commands =
    commands
        |> applyCommandsToRopeRecordTail (makeRope 10 [])


outputFromString : String -> ( Rope, Set ( Int, Int ) )
outputFromString commands =
    commands
        |> parseCommandsFromInput
        |> applyCommandsToRopeRecordTail (makeRope 10 [])


main : Html msg
main =
    realInput
        |> parseCommandsFromInput
        |> applyCommandsToRope (makeRope 10 [])
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
