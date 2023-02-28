module Advent1Test exposing (..)

import Advent1 exposing (..)
import Expect
import Test exposing (..)


testInput : String
testInput =
    """1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"""


suite : Test
suite =
    describe "Advent Day 1"
        [ describe "Parse input"
            [ test "Parse input into lists" <|
                \_ ->
                    Expect.equal
                        (Just 24000)
                        (advent1Part1 testInput)
            ]
        ]
