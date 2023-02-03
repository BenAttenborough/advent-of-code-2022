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
    , head : Coordinates
    , visited : List Coordinates
    }


initialRopeState : Rope
initialRopeState =
    { tail = Coordinates 0 0
    , head = Coordinates 0 0
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
        |> List.foldl
            (\command rope ->
                let
                    tailX =
                        coordinatesX rope.tail

                    tailY =
                        coordinatesY rope.tail

                    relHeadPos =
                        case command of
                            Up distance ->
                                Coordinates
                                    (coordinatesX rope.head)
                                    (coordinatesY rope.head + distance)

                            Down distance ->
                                Coordinates
                                    (coordinatesX rope.head)
                                    (coordinatesY rope.head - distance)

                            Left distance ->
                                Coordinates
                                    (coordinatesX rope.head - distance)
                                    (coordinatesY rope.head)

                            Right distance ->
                                Coordinates
                                    (coordinatesX rope.head + distance)
                                    (coordinatesY rope.head)
                in
                case command of
                    Up distance ->
                        { rope | tail = Coordinates tailX (tailY + distance) }

                    Down distance ->
                        { rope | tail = Coordinates tailX (tailY - distance) }

                    Right distance ->
                        { rope | tail = Coordinates (tailX + distance) tailY }

                    Left distance ->
                        { rope | tail = Coordinates (tailX - distance) tailY }
            )
            initialRopeState
        |> Debug.toString
        |> Html.text


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
