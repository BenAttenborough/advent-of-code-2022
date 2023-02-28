module Data.{{ name }}Test exposing (..)

import {{ name }} exposing (..)
import {{ name }}Data exposing (realInput, testInput)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "{{ name }}"
        [ describe "Part 1" <|
            [ test "Hello World" <|
                \_ ->
                    Expect.equal
                        1
                        1
            ]
        ]
