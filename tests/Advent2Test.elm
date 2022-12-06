module Advent2Test exposing (..)

import Advent2 exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser exposing (Parser)
import Test exposing (..)


suite : Test
suite =
    describe "Advent Day 2"
        [ describe "Dummy test"
            [ test "Example" <|
                \_ -> Expect.equal True True
            ]
        , describe "attackParser"
            [ test "Parse 'A' to 'Rock' type" <|
                \_ -> Expect.equal (Ok Rock) (Parser.run attackParser "A X")
            ]
        , describe "generalParser"
            [ test "Parse 'A X' to '{attack: Rock, defense: Rock}' type" <|
                \_ -> Expect.equal (Ok { attack = Rock, defense = Rock }) (Parser.run generalParser "A X")
            ]
        ]
