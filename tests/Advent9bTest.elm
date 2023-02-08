module Advent9bTest exposing (..)

import Advent9Data exposing (testInput)
import Advent9b exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser exposing (Parser)
import Set exposing (Set)
import Test exposing (..)


suite : Test
suite =
    describe "Advent Day 9"
        [ describe "Part 1" <|
            [ test "createNCommands creates 3 UP comands with appropriate input" <|
                \_ ->
                    Expect.equal
                        (createNCommands 3 Up [])
                        [ Up, Up, Up ]
            , test "createNCommands creates 1 UP comands with appropriate input" <|
                \_ ->
                    Expect.equal
                        (createNCommands 1 Up [])
                        [ Up ]
            , test "createNCommands creates 0 comands with 0 input" <|
                \_ ->
                    Expect.equal
                        (createNCommands 0 Up [])
                        []
            , test "createNCommands creates 0 comands with negative input" <|
                \_ ->
                    Expect.equal
                        (createNCommands -1 Up [])
                        []
            , test "commandParser Up 3 creates 3 UP comands" <|
                \_ ->
                    Expect.equal
                        (Parser.run commandParser "U 3")
                        (Ok [ Up, Up, Up ])
            , test "makeRope 10 produces a list of size 10" <|
                \_ ->
                    Expect.equal
                        (makeRope 10 []
                            |> List.length
                        )
                        10
            ]
        ]
