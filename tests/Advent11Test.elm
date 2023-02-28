module Advent11Test exposing (..)

import Advent11 exposing (..)
import Data.Advent11Data exposing (realInput, testInput)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Advent Day 11"
        [ describe "Part 1" <|
            [ test "Hello World" <|
                \_ ->
                    Expect.equal
                        1
                        1
            ]
        ]
