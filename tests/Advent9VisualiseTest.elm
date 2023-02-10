module Advent9VisualiseTest exposing (..)

import Advent9Visualise exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Advent Day 9 Visualisation"
        [ describe "Part 1" <|
            [ test "create row 10 wide" <|
                \_ ->
                    Expect.equal
                        [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
                        (makeRow 10 [])
            , test "create row 0 wide" <|
                \_ ->
                    Expect.equal
                        []
                        (makeRow 0 [])
            , test "create row -1 wide" <|
                \_ ->
                    Expect.equal
                        []
                        (makeRow -1 [])
            , test "create playfield 3x3" <|
                \_ ->
                    Expect.equal
                        [ [ Empty, Empty, Empty ], [ Empty, Empty, Empty ], [ Empty, Empty, Empty ] ]
                        (makePlayfield 3 3 [])
            , test "create playfield 0x3" <|
                \_ ->
                    Expect.equal
                        [ [], [], [] ]
                        (makePlayfield 0 3 [])
            , test "create playfield -1x3" <|
                \_ ->
                    Expect.equal
                        [ [], [], [] ]
                        (makePlayfield -1 3 [])
            , test "create playfield 3x0" <|
                \_ ->
                    Expect.equal
                        []
                        (makePlayfield 3 0 [])
            , test "create playfield 3x-1" <|
                \_ ->
                    Expect.equal
                        []
                        (makePlayfield 3 -1 [])
            ]
        ]
