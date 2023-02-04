module Advent9 exposing (..)

import Advent9Data exposing (testInput)
import Html exposing (Html, p, text)
import Json.Decode exposing (oneOf)
import Maybe.Extra exposing (isJust)
import Parser exposing (..)


type Command
    = Up Int
    | Down Int
    | Left Int
    | Right Int


type Coordinates
    = Coordinates Int Int


tailPosition : Coordinates
tailPosition =
    Coordinates 0 0


relativeHeadPosition : Coordinates
relativeHeadPosition =
    Coordinates 0 0


positionsTailVisited : List Coordinates
positionsTailVisited =
    [ Coordinates 0 0 ]


type alias Rope =
    { tail : Coordinates
    , headRel : Coordinates
    , visited : List Coordinates
    }


initialRopeState : Rope
initialRopeState =
    { tail = Coordinates 0 0
    , headRel = Coordinates 0 0
    , visited = [ Coordinates 0 0 ]
    }


coordinatesX : Coordinates -> Int
coordinatesX coords =
    case coords of
        Coordinates x _ ->
            x


coordinatesY : Coordinates -> Int
coordinatesY coords =
    case coords of
        Coordinates _ y ->
            y


main : Html msg
main =
    testInput
        |> parseCommandsFromInput
        |> List.foldl applyCommandsToRopeState
            initialRopeState
        |> Debug.toString
        |> Html.text


intermediateRelativeHeadPosition : Command -> Rope -> Coordinates
intermediateRelativeHeadPosition command rope =
    case command of
        Up distance ->
            Coordinates
                (coordinatesX rope.headRel)
                (coordinatesY rope.headRel + distance)

        Down distance ->
            Coordinates
                (coordinatesX rope.headRel)
                (coordinatesY rope.headRel - distance)

        Left distance ->
            Coordinates
                (coordinatesX rope.headRel - distance)
                (coordinatesY rope.headRel)

        Right distance ->
            Coordinates
                (coordinatesX rope.headRel + distance)
                (coordinatesY rope.headRel)


finalRelativeHeadPosition : Command -> Rope -> Coordinates
finalRelativeHeadPosition command rope =
    case command of
        Up distance ->
            Coordinates
                (coordinatesX rope.headRel)
                (min 1 (coordinatesY rope.headRel + distance))

        Down distance ->
            Coordinates
                (coordinatesX rope.headRel)
                (min -1 (coordinatesY rope.headRel - distance))

        Left distance ->
            Coordinates
                (coordinatesX rope.headRel - distance)
                (min -1 (coordinatesY rope.headRel))

        Right distance ->
            Coordinates
                (min 1 (coordinatesX rope.headRel + distance))
                (coordinatesY rope.headRel)


applyCommandsToRopeState : Command -> Rope -> Rope
applyCommandsToRopeState command rope =
    let
        tailX =
            coordinatesX rope.tail

        tailY =
            coordinatesY rope.tail

        headRelX =
            coordinatesX rope.headRel

        headRelY =
            coordinatesY rope.headRel

        relHeadPos =
            -- this is not right
            case command of
                Up distance ->
                    Coordinates
                        (coordinatesX rope.headRel)
                        (coordinatesY rope.headRel + distance)

                Down distance ->
                    Coordinates
                        (coordinatesX rope.headRel)
                        (coordinatesY rope.headRel - distance)

                Left distance ->
                    Coordinates
                        (coordinatesX rope.headRel - distance)
                        (coordinatesY rope.headRel)

                Right distance ->
                    Coordinates
                        (coordinatesX rope.headRel + distance)
                        (coordinatesY rope.headRel)
    in
    { tail = Coordinates 0 0
    , headRel = finalRelativeHeadPosition command rope
    , visited = tailLocationsVisited command rope.tail rope.visited
    }



-- Needs working on


calcTailPos : Coordinates -> Coordinates -> Coordinates
calcTailPos current headPos =
    Coordinates 0 0


tailLocationsVisited : Command -> Coordinates -> List Coordinates -> List Coordinates
tailLocationsVisited command currentLocation visitedLocations =
    case command of
        Up distance ->
            if distance <= 1 then
                visitedLocations

            else
                let
                    newCoordinates =
                        Coordinates 0 (coordinatesX currentLocation + 1)
                in
                tailLocationsVisited command newCoordinates (currentLocation :: visitedLocations)

        Down distance ->
            if distance <= 1 then
                visitedLocations

            else
                let
                    newCoordinates =
                        Coordinates 0 (coordinatesX currentLocation - 1)
                in
                tailLocationsVisited command newCoordinates (currentLocation :: visitedLocations)

        Right distance ->
            if distance <= 1 then
                visitedLocations

            else
                let
                    newCoordinates =
                        Coordinates (coordinatesX currentLocation + 1) 0
                in
                tailLocationsVisited command newCoordinates (currentLocation :: visitedLocations)

        Left distance ->
            if distance <= 1 then
                visitedLocations

            else
                let
                    newCoordinates =
                        Coordinates (coordinatesX currentLocation - 1) 0
                in
                tailLocationsVisited command newCoordinates (currentLocation :: visitedLocations)


parseCommandsFromInput : String -> List Command
parseCommandsFromInput input =
    input
        |> String.lines
        |> List.map (Parser.run commandParser)
        |> List.map Result.toMaybe
        |> List.filterMap identity


commandParser : Parser Command
commandParser =
    succeed identity
        |= Parser.oneOf
            [ succeed Up
                |. keyword "U"
                |. spaces
                |= int
            , succeed Down
                |. keyword "D"
                |. spaces
                |= int
            , succeed Left
                |. keyword "L"
                |. spaces
                |= int
            , succeed Right
                |. keyword "R"
                |. spaces
                |= int
            ]
