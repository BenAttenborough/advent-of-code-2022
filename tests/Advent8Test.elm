module Advent8Test exposing (..)

import Advent8 exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser exposing (Parser)
import Test exposing (..)


testInput : String
testInput =
    """30373
25512
65332
33549
35390"""


suite : Test
suite =
    -- let
    --     _ =
    --         Debug.log "day8Part1"
    --             (day8Part1 testInput)
    -- in
    describe "Advent Day 8"
        [ describe "Part 1" <|
            [ test "findVisibleTrees 3 in list [ 1, 3, 3, 2, 5 ]" <|
                \_ ->
                    Expect.equal
                        [ ( 1, True ), ( 3, True ), ( 3, False ), ( 2, False ), ( 5, True ) ]
                        (findVisibleTrees
                            -1
                            []
                            [ ( 1, False )
                            , ( 3, False )
                            , ( 3, False )
                            , ( 2, False )
                            , ( 5, False )
                            ]
                        )
            , test "day8Part1" <|
                \_ -> Expect.equal (Just 21) (day8Part1 testInput)
            ]
        ]
