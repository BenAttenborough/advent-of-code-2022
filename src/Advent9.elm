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


type alias TailPosition =
    { x : Int
    , y : Int
    }


type alias HeadRelativePosition =
    { x : Int
    , y : Int
    }


tailPosition : ( Int, Int )
tailPosition =
    ( 0, 0 )


relativeHeadPosition : ( Int, Int )
relativeHeadPosition =
    ( 0, 0 )


initialHeadRelativePosition =
    HeadRelativePosition 0 0


main : Html msg
main =
    testInput
        |> parseCommandsFromInput
        |> List.foldl
            (\command position ->
                case command of
                    Up distance ->
                        -- let
                        --     relativePosition
                        -- in
                        { position | y = position.y + distance }

                    Down distance ->
                        { position | y = position.y - distance }

                    Right distance ->
                        { position | x = position.x + distance }

                    Left distance ->
                        { position | x = position.x - distance }
            )
            tailPosition
        |> Debug.toString
        |> Html.text


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
