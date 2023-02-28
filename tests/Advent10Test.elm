module Advent10Test exposing (..)

import Advent10 exposing (..)
import Array
import Data.Advent10Data exposing (part1Data, testData)
import Expect
import Parser exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Advent Day 10"
        [ describe "Part 1" <|
            [ test "Parse 'noop' string" <|
                \_ ->
                    Expect.equal
                        (Parser.run commandParser "noop")
                        (Ok NoOp)
            , test "Parse 'addx 5' string" <|
                \_ ->
                    Expect.equal
                        (Parser.run commandParser "addx 5")
                        (Ok (AddX 5))
            , test "Parse 'addx -5' string" <|
                \_ ->
                    Expect.equal
                        (Parser.run commandParser "addx -5")
                        (Ok (AddX -5))
            , test "'processCommand3 (AddX 5)' to register of [5]" <|
                \_ ->
                    Expect.equal
                        (processCommand
                            (AddX 5)
                            { register = Array.fromList [ 5 ]
                            , lastCommandResult = 5
                            }
                        )
                        { register = Array.fromList [ 5, 5, 5 ]
                        , lastCommandResult = 10
                        }
            , test "processCommands3 [(AddX 5), NoOp] to register of [5]" <|
                \_ ->
                    Expect.equal
                        (processCommands
                            { register = Array.fromList [ 5 ]
                            , lastCommandResult = 5
                            }
                            [ AddX 5, NoOp ]
                        )
                        { register = Array.fromList [ 5, 5, 5, 10 ]
                        , lastCommandResult = 10
                        }
            , test "Signal strength from index" <|
                \_ ->
                    Expect.equal
                        (signalStrengthFromIndex 5 (Array.fromList [ 1, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]))
                        25
            , test "getIndexes [5,6] [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]" <|
                \_ ->
                    Expect.equal
                        (mapSelectedIndexes
                            [ 5, 6 ]
                            (\a _ -> a)
                            (Array.fromList [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ])
                        )
                        [ 5, 6 ]
            , test "Sum of testData signal strengths is 13140" <|
                \_ ->
                    Expect.equal
                        (answerPart1 testData)
                        13140
            , test "Sum of part1Data signal strengths is 14520" <|
                \_ ->
                    Expect.equal
                        (answerPart1 part1Data)
                        14520

            -- , test "answerPart2" <|
            --     \_ ->
            --         Expect.equal
            --             (answerPart2 testData)
            --             ""
            -- , test "debug" <|
            --     \_ ->
            --         Expect.equal
            --             (debug testData)
            --             []
            -- , test "answerPart2 final" <|
            --     \_ ->
            --         Expect.equal
            --             (answerPart2 part1Data)
            --             "."
            ]
        ]
