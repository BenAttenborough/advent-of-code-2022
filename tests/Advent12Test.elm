module Advent12Test exposing (..)

import Advent12 exposing (..)
import Array exposing (Array)
import Data.Advent12Data exposing (testInput)
import Expect
import Html.Attributes exposing (type_)
import Test exposing (..)
import Tree exposing (Tree, tree)
import Tree.Zipper as Zipper


testTwoDArray : Array (Array Int)
testTwoDArray =
    Array.fromList
        [ Array.fromList [ 0, 1, 2 ]
        , Array.fromList [ 3, 4, 5 ]
        , Array.fromList [ 6, 7, 8 ]
        ]


suite : Test
suite =
    describe "Advent12"
        [ describe "Part 1" <|
            [ test "Hello World" <|
                \_ ->
                    Expect.equal
                        1
                        1
            , test "prepareInput" <|
                \_ ->
                    Expect.equal
                        (prepareInput "aSa\nbEb")
                        (Array.fromList
                            [ Array.fromList
                                [ { cellType = Journey, elevation = 0 }
                                , { cellType = Start, elevation = startElevation }
                                , { cellType = Journey, elevation = 0 }
                                ]
                            , Array.fromList
                                [ { cellType = Journey, elevation = 1 }
                                , { cellType = End, elevation = endElevation }
                                , { cellType = Journey, elevation = 1 }
                                ]
                            ]
                        )
            , test "prepareInput testInput" <|
                \_ ->
                    -- Comparing all the records is tedious so we just compare elevations
                    Expect.equal
                        (prepareInput testInput |> Array.map (Array.map .elevation))
                        (Array.fromList
                            [ Array.fromList [ startElevation, 0, 1, 16, 15, 14, 13, 12 ]
                            , Array.fromList [ 0, 1, 2, 17, 24, 23, 23, 11 ]
                            , Array.fromList [ 0, 2, 2, 18, 25, endElevation, 23, 10 ]
                            , Array.fromList [ 0, 2, 2, 19, 20, 21, 22, 9 ]
                            , Array.fromList [ 0, 1, 3, 4, 5, 6, 7, 8 ]
                            ]
                        )
            , test "charToCode a" <|
                \_ ->
                    Expect.equal
                        (charToCode 'a')
                        (Cell 0 Journey)
            , test "charToCode b" <|
                \_ ->
                    Expect.equal
                        (charToCode 'b')
                        (Cell 1 Journey)
            , test "charToCode z" <|
                \_ ->
                    Expect.equal
                        (charToCode 'z')
                        (Cell 25 Journey)
            , test "charToCode S" <|
                \_ ->
                    Expect.equal
                        (charToCode 'S')
                        (Cell startElevation Start)
            , test "charToCode E" <|
                \_ ->
                    Expect.equal
                        (charToCode 'E')
                        (Cell endElevation End)
            , test "simpleArrayTarverse" <|
                \_ ->
                    Expect.equal
                        (simpleArrayTarverse 0 Array.empty (Array.fromList [ 'a', 'b', 'c' ]))
                        (Array.fromList [ 'a', 'b', 'c' ])
            , test "simpleArrayTarverse 1" <|
                \_ ->
                    Expect.equal
                        (simpleArrayTarverse 1 Array.empty (Array.fromList [ 'a', 'b', 'c' ]))
                        (Array.fromList [ 'b', 'c' ])
            , test "simpleArrayTarverse out of bounds" <|
                \_ ->
                    Expect.equal
                        (simpleArrayTarverse 5 Array.empty (Array.fromList [ 'a', 'b', 'c' ]))
                        Array.empty
            , test "simpleArrayTarverse out of bounds -5" <|
                \_ ->
                    Expect.equal
                        (simpleArrayTarverse -5 Array.empty (Array.fromList [ 'a', 'b', 'c' ]))
                        Array.empty

            -- , test "simpleArrayTarverseTwo" <|
            --     \_ ->
            --         Expect.equal
            --             (simpleArrayTarverseTwo
            --                 0
            --                 (Zipper.fromTree (Tree.singleton 'z'))
            --                 (Array.fromList [ 'a', 'b', 'c' ])
            --             )
            --             (Zipper.fromTree (Tree.singleton 'z'))
            , test "getNode 1 1 testTwoDArray = Just 4" <|
                \_ ->
                    Expect.equal
                        (getNode 1 1 testTwoDArray)
                        (Just 4)
            , test "getNode 0 0 testTwoDArray = Just 0" <|
                \_ ->
                    Expect.equal
                        (getNode 0 0 testTwoDArray)
                        (Just 0)
            , test "getNode 3 0 testTwoDArray = Nothing" <|
                \_ ->
                    Expect.equal
                        (getNode 3 0 testTwoDArray)
                        Nothing
            , test "getNode -1 -1 testTwoDArray = Nothing" <|
                \_ ->
                    Expect.equal
                        (getNode -1 -1 testTwoDArray)
                        Nothing
            , test "nodeTraversable 1 1 True" <|
                \_ ->
                    Expect.equal
                        (nodeTraversable { elevation = 1, cellType = Journey } { elevation = 1, cellType = Journey })
                        True
            , test "nodeTraversable 1 2 True" <|
                \_ ->
                    Expect.equal
                        (nodeTraversable { elevation = 1, cellType = Journey } { elevation = 2, cellType = Journey })
                        True
            , test "nodeTraversable 1 3 False" <|
                \_ ->
                    Expect.equal
                        (nodeTraversable { elevation = 1, cellType = Journey } { elevation = 3, cellType = Journey })
                        False
            , test "nodeTraversable 5 3 False" <|
                \_ ->
                    Expect.equal
                        (nodeTraversable { elevation = 5, cellType = Journey } { elevation = 3, cellType = Journey })
                        False
            , test "nodeTraversable 5 4 True" <|
                \_ ->
                    Expect.equal
                        (nodeTraversable { elevation = 5, cellType = Journey } { elevation = 4, cellType = Journey })
                        True
            , test "getNodes" <|
                \_ ->
                    Expect.equal
                        (getNodes 1 1 testTwoDArray)
                        [ 1, 7, 3, 5 ]
            , test "getNodes 0 0 testTwoDArray" <|
                \_ ->
                    Expect.equal
                        (getNodes 0 0 testTwoDArray)
                        [ 3, 1 ]
            ]
        ]
