module Advent10 exposing (..)

import Array exposing (Array)
import Parser exposing (..)
import Utilities.Utilities exposing (partitioner)


type Command
    = NoOp
    | AddX Int


initX : Int
initX =
    1


type alias State =
    { register : Array Int
    , lastCommandResult : Int
    }


initRegisterState : State
initRegisterState =
    State Array.empty 0


indexesToCheck : List Int
indexesToCheck =
    [ 20, 60, 100, 140, 180, 220 ]


commandParser : Parser Command
commandParser =
    succeed identity
        |= Parser.oneOf
            [ succeed NoOp
                |. keyword "noop"
                |. spaces
            , succeed AddX
                |. keyword "addx"
                |. spaces
                |= oneOf
                    [ succeed negate
                        |. symbol "-"
                        |= int
                    , int
                    ]
            ]


parseCommandsFromInput : String -> List Command
parseCommandsFromInput input =
    input
        |> String.lines
        |> List.map (Parser.run commandParser)
        |> List.map Result.toMaybe
        |> List.filterMap identity


processCommand : Command -> State -> State
processCommand command state =
    case command of
        NoOp ->
            { register = Array.push state.lastCommandResult state.register
            , lastCommandResult = state.lastCommandResult
            }

        AddX val ->
            { register = Array.append state.register (Array.fromList [ state.lastCommandResult, state.lastCommandResult ])
            , lastCommandResult = state.lastCommandResult + val
            }


processCommands : State -> List Command -> State
processCommands state commands =
    commands
        |> List.foldl processCommand state


signalStrengthFromIndex : Int -> Array Int -> Int
signalStrengthFromIndex index register =
    register
        |> Array.get index
        |> Maybe.withDefault 0
        |> (*) index


mapSelectedIndexes : List Int -> (Int -> Array Int -> Int) -> Array Int -> List Int
mapSelectedIndexes indexes function register =
    indexes
        |> List.foldl
            (\index signals ->
                let
                    signal =
                        function index register
                in
                List.append signals [ signal ]
            )
            []


answerPart1 : String -> Int
answerPart1 input =
    input
        |> parseCommandsFromInput
        |> processCommands
            { register = Array.fromList [ initX ]
            , lastCommandResult = initX
            }
        |> (\state ->
                mapSelectedIndexes indexesToCheck signalStrengthFromIndex state.register
           )
        |> List.foldl (+) 0


answerPart2 : String -> String
answerPart2 input =
    input
        |> parseCommandsFromInput
        |> processCommands
            { register = Array.fromList []
            , lastCommandResult = initX
            }
        |> .register
        |> Array.toList
        |> List.indexedMap decodeRegister
        |> partitioner 40 []
        |> List.map (\line -> List.append line [ "\n" ])
        |> List.concat
        |> String.concat


decodeRegister : Int -> Int -> String
decodeRegister index signal =
    let
        spriteRange =
            List.range (signal - 1) (signal + 1)
    in
    if List.member (modBy 40 index) spriteRange then
        "#"

    else
        "."


debug : String -> List ( Int, Int )
debug input =
    input
        |> parseCommandsFromInput
        |> processCommands
            { register = Array.fromList []
            , lastCommandResult = initX
            }
        |> .register
        |> Array.toIndexedList
