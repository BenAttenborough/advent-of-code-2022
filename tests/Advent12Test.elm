module Advent12Test exposing (..)

-- import Array exposing (Array)
-- import Dict
-- import Data.Advent12Data exposing (difficultInput, pruningInput, realInput, simplifiedInput, testInput)

import Advent12 exposing (..)
import Expect
import Test exposing (..)


baseTests : List Test
baseTests =
    []


devTests : List Test
devTests =
    [ test "foo" <|
        \_ -> Expect.equal 1 1
    ]


solutionsTests : List Test
solutionsTests =
    [ test "null" <|
        \_ ->
            Expect.equal
                1
                1
    ]


suite : Test
suite =
    describe "Advent12"
        [ describe "Part 1" <|
            [ describe "Base tests" <| baseTests
            , describe "Dev tests" <| devTests
            , only <| describe "Test solutions" <| solutionsTests
            ]
        ]
