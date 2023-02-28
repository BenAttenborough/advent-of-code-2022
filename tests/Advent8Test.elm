module Advent8Test exposing (..)

import Advent8 exposing (..)
import Data.Advent8Data exposing (day8Part1Data)
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


listTreeExample : List Tree
listTreeExample =
    [ { height = 3, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    , { height = 0, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    , { height = 3, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    , { height = 7, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    , { height = 3, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    ]


listTreeExample2 : List Tree
listTreeExample2 =
    [ { height = 5, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    , { height = 0, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    , { height = 3, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    , { height = 4, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    , { height = 3, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    ]


listTreeAoCExample : List Tree
listTreeAoCExample =
    [ { height = 3, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    , { height = 3, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    , { height = 5, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    , { height = 4, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    , { height = 9, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    ]


listTreeFiveFourNine =
    [ { height = 5, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    , { height = 4, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    , { height = 9, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    ]


listTreeOneTree : List Tree
listTreeOneTree =
    [ { height = 3, treesSeen = { east = 0, north = 0, south = 0, west = 0 } }
    ]


listTreeEmpty : List Tree
listTreeEmpty =
    []


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
        , describe "Part 2 seenTrees 1" <|
            [ test "seenTrees in [3,0,3,7,3] expected to be 1" <|
                \_ ->
                    Expect.equal (Just 2) (seenTrees listTreeExample)
            ]
        , describe "Part 2  seenTrees 2" <|
            [ test "seenTrees in [5,0,3,4,3] expected to be 4" <|
                \_ ->
                    Expect.equal (Just 4) (seenTrees listTreeExample2)
            ]
        , describe "Part 2 seenTrees listTreeAoCExample" <|
            [ test "seenTrees in [3,3,5,4,9] expected to be 1" <|
                \_ ->
                    Expect.equal (Just 1) (seenTrees listTreeAoCExample)
            ]
        , describe "Part 2 seenTrees listTreeAoCExample reversed" <|
            [ test "seenTrees in [9,4,5,3,3] expected to be 4" <|
                \_ ->
                    Expect.equal (Just 4) (seenTrees (List.reverse listTreeAoCExample))
            ]
        , describe "Part 2 listTreeFiveFourNine from 5" <|
            [ test "seenTrees in [5,4,9] expected to be 2" <|
                \_ ->
                    Expect.equal (Just 2) (seenTrees listTreeFiveFourNine)
            ]
        , describe "Part 2 seenTrees 1 tree" <|
            [ test "seenTrees in [3] expected to be 0" <|
                \_ ->
                    Expect.equal (Just 0) (seenTrees listTreeOneTree)
            ]
        , describe "Part 2 seenTrees 0 trees" <|
            [ test "seenTrees in [] expected to be Nothing" <|
                \_ ->
                    Expect.equal Nothing (seenTrees listTreeEmpty)
            ]
        , describe "Part 2 day8Part2 testinput" <|
            [ test "day8Part2 testinput = 8" <|
                \_ ->
                    Expect.equal 8 (day8Part2 testInput)
            ]
        , describe "Part 2 day8Part2 day8Part1Data" <|
            [ test "day8Part2 testinput = 201684" <|
                \_ ->
                    Expect.equal 201684 (day8Part2 day8Part1Data)
            ]
        ]
