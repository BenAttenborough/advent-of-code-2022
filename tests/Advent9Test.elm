module Advent9Test exposing (..)

import Advent9 exposing (..)
import Advent9Data exposing (testInput)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser exposing (Parser)
import Test exposing (..)


exampleCommand : String
exampleCommand =
    "U 10"


suite : Test
suite =
    describe "Advent Day 9"
        [ describe "Part 1" <|
            [ test "Expect commandParser exampleCommand to equal Up 10" <|
                \_ ->
                    Expect.equal
                        (Parser.run commandParser exampleCommand)
                        (Ok (Up 10))
            , test "Parsing test input gives expected output" <|
                \_ ->
                    Expect.equal
                        (parseCommandsFromInput testInput)
                        [ Right 4, Up 4, Left 3, Down 1, Right 4, Down 1, Left 5, Right 2 ]
            , test "intermediateRelativeHeadPosition updates relative head position correctly" <|
                \_ ->
                    Expect.equal
                        (intermediateRelativeHeadPosition (Up 3) initialRopeState)
                        (Coordinates 0 3)
            , test "intermediateRelativeHeadPosition works for down" <|
                \_ ->
                    Expect.equal
                        (intermediateRelativeHeadPosition (Down 3) initialRopeState)
                        (Coordinates 0 -3)
            , test "intermediateRelativeHeadPosition down 4 from up 1" <|
                \_ ->
                    Expect.equal
                        (intermediateRelativeHeadPosition
                            (Down 4)
                            { tail = Coordinates 0 0
                            , headRel = Coordinates 0 1
                            , visited = [ Coordinates 0 0 ]
                            }
                        )
                        (Coordinates 0 -3)
            , test "finalRelativeHeadPosition updates relative head position correctly" <|
                \_ ->
                    Expect.equal
                        (finalRelativeHeadPosition (Up 3) initialRopeState)
                        (Coordinates 0 1)
            , test "finalRelativeHeadPosition down 3 decomes Coordinated 0 -1" <|
                \_ ->
                    Expect.equal
                        (finalRelativeHeadPosition (Down 3) initialRopeState)
                        (Coordinates 0 -1)

            -- , test "calcTailPos (Coordinates 0 0) (Coordinates 3 0) becomes Coordinated 2 0" <|
            --     \_ ->
            --         Expect.equal
            --             (calcTailPos (Coordinates 0 0) (Coordinates 3 0))
            --             (Coordinates 2 0)
            ]
        ]
