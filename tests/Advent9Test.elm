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
            ]
        ]
