module Advent9b exposing (..)

import Advent9Data exposing (realInput, testInput)
import Html exposing (Html)
import Json.Decode exposing (list)
import List exposing (foldl)
import List.Extra
import Parser exposing (..)
import Set exposing (Set)


type Command
    = Up
    | Down
    | Left
    | Right


type alias Rope =
    List ( Int, Int )


type alias RopeState =
    { positions : Rope
    , tailVisited : Set ( Int, Int )
    }


initRopeState : RopeState
initRopeState =
    RopeState (makeRope 10 []) (Set.fromList [ ( 0, 0 ) ])


makeRope : Int -> Rope -> Rope
makeRope size rope =
    if size < 1 then
        rope

    else
        makeRope (size - 1) (( 0, 0 ) :: rope)


relativePositionToTransform : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
relativePositionToTransform ( firstX, firstY ) ( secondX, secondY ) =
    -- Slightly painful logic to determine how a knot should move relative to the previous knot's position
    let
        relX =
            secondX - firstX

        relY =
            secondY - firstY
    in
    if relX > 1 then
        if relY > 0 then
            ( 1, 1 )

        else if relY == 0 then
            ( 1, 0 )

        else
            ( 1, -1 )

    else if relX < -1 then
        if relY > 0 then
            ( -1, 1 )

        else if relY == 0 then
            ( -1, 0 )

        else
            ( -1, -1 )

    else if relY > 1 then
        -- relx == -1, 0 or 1
        ( relX, 1 )

    else if relY < -1 then
        ( relX, -1 )

    else
        ( 0, 0 )


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
        ( newX, newY ) =
            commandToTransform command
    in
    ( x + newX, y + newY )


moveKnotRelativeToLast : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
moveKnotRelativeToLast ( lastX, lastY ) ( curX, curY ) =
    -- Key insight: direction isn't going to help with knots further down the chain
    let
        ( newX, newY ) =
            relativePositionToTransform ( curX, curY ) ( lastX, lastY )
    in
    ( curX + newX, curY + newY )


applyCommandsToRope : RopeState -> List Command -> RopeState
applyCommandsToRope rope commands =
    commands
        |> foldl
            (\command acc ->
                let
                    intermediateRope =
                        applyCommandToRope command acc.positions
                in
                RopeState
                    intermediateRope
                    (intermediateRope
                        |> List.Extra.last
                        |> Maybe.withDefault ( 0, 0 )
                        |> (\x -> Set.insert x acc.tailVisited)
                    )
            )
            rope


applyCommandToRope : Command -> Rope -> Rope
applyCommandToRope command rope =
    rope
        |> foldl
            (\currentKnot acc ->
                case acc of
                    [] ->
                        List.append acc [ moveKnot command currentKnot ]

                    x :: xs ->
                        let
                            -- Some dodgy logic here!
                            lastKnot =
                                List.Extra.last xs
                                    |> Maybe.withDefault x

                            newKnot =
                                moveKnotRelativeToLast lastKnot currentKnot
                        in
                        List.append acc [ newKnot ]
            )
            []


main : Html msg
main =
    testInput
        |> parseCommandsFromInput
        |> applyCommandsToRope initRopeState
        |> (\ropeState -> ropeState.tailVisited |> (\x -> List.length (Set.toList x)))
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
