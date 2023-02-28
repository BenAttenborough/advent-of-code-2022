module Advent12Test exposing (..)

import Advent12 exposing (..)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Advent12"
        [ describe "Part 1" <|
            [ test "Hello World" <|
                \_ ->
                    Expect.equal
                        1
                        1
            ]
        ]
