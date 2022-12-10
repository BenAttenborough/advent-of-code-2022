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
    --         Debug.log "Advent Day 8" (day8Part1 testInput)
    -- in
    describe "Advent Day 8"
        [ describe "Part 1" <|
            [ test "Example" <|
                \_ -> Expect.equal True True

            -- , test "numberOfVisibleTrees 5 in list [ 1, 2, 3, 4, 5 ]" <|
            --     \_ ->
            --         Expect.equal
            --             5
            --             (numberOfVisibleTrees [ 1, 2, 3, 4, 5 ] -1 0)
            -- , test "numberOfVisibleTrees 1 in list [ 5, 2, 3, 4, 5 ]" <|
            --     \_ ->
            --         Expect.equal
            --             1
            --             (numberOfVisibleTrees [ 5, 2, 3, 4, 5 ] -1 0)
            -- , test "numberOfVisibleTrees 3 in list [ 1, 3, 3, 2, 5 ]" <|
            --     \_ ->
            --         Expect.equal
            --             3
            --             (numberOfVisibleTrees [ 1, 3, 3, 2, 5 ] -1 0)
            , test "numberOfVisibleTrees 3 in list [ 1, 3, 3, 2, 5 ]" <|
                \_ ->
                    Expect.equal
                        ( 3, [ ( 1, True ), ( 3, True ), ( 3, False ), ( 2, False ), ( 5, True ) ] )
                        (numberOfVisibleTrees
                            [ ( 1, False )
                            , ( 3, False )
                            , ( 3, False )
                            , ( 2, False )
                            , ( 5, False )
                            ]
                            -1
                            0
                            []
                        )

            -- , test "day8Part1" <|
            --     \_ -> Expect.equal 21 day8Part1 testInput
            ]
        ]



-- numberOfVisibleTrees [ ( 1, False ), ( 3, False ), ( 3, False ), ( 2, False ), ( 5, False ) ] -1 0 []
