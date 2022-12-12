module AlternativeSolutionTests.Advent1Test exposing (..)

import AlternativeSolutions.Advent1 exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


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


testInput2 =
    """1000
2000
3000"""


suite : Test
suite =
    describe "Advent Day 1"
        [ describe "Dummy test"
            [ test "Example" <|
                \_ -> Expect.equal True True
            ]
        , describe "getBiggestRations"
            [ test "Test input results in 24000" <|
                \_ ->
                    Expect.equal
                        (Ok 24000)
                        (getBiggestRations testInput)
            ]
        , describe "getTopThreeRations"
            [ test "Test input results in 45000" <|
                \_ ->
                    Expect.equal
                        (Ok 45000)
                        (getTopThreeRations testInput)
            ]

        -- , describe "Parse input"
        --     [ test "Parse input into lists" <|
        --         \_ ->
        --             Expect.equal
        --                 (Ok [ "111" ])
        --                 (parseInput testInput2)
        --     ]
        , describe "Parse input"
            [ test "Parse input into lists" <|
                \_ ->
                    Expect.equal
                        24000
                        (advent1Part1 testInput)
            ]
        ]
