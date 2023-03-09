module Advent12Test exposing (..)

import Advent12 exposing (..)
import Array exposing (Array)
import Data.Advent12Data exposing (testInput)
import Dict
import Expect
import Test exposing (..)



-- import Tree.Zipper as Zipper


testTwoDArray : Array (Array Int)
testTwoDArray =
    Array.fromList
        [ Array.fromList [ 0, 1, 2 ]
        , Array.fromList [ 3, 4, 5 ]
        , Array.fromList [ 6, 7, 8 ]
        ]


testCellMap : Array (Array Cell)
testCellMap =
    Array.fromList
        [ Array.fromList [ Cell 5 Journey 0 0, Cell 2 Journey 1 0, Cell 5 Journey 2 0 ]
        , Array.fromList [ Cell 2 Journey 0 1, Cell 1 Start 1 1, Cell 5 Journey 2 1 ]
        , Array.fromList [ Cell 5 Journey 0 2, Cell 5 Journey 1 2, Cell 5 End 2 2 ]
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
                                [ { cellType = Journey, elevation = 0, x = 0, y = 0 }
                                , { cellType = Start, elevation = 0, x = 1, y = 0 }
                                , { cellType = Journey, elevation = 0, x = 2, y = 0 }
                                ]
                            , Array.fromList
                                [ { cellType = Journey, elevation = 1, x = 0, y = 1 }
                                , { cellType = End, elevation = 25, x = 1, y = 1 }
                                , { cellType = Journey, elevation = 1, x = 2, y = 1 }
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
            , test "prepareInput start end - tests that start and end types have correct height" <|
                \_ ->
                    Expect.equal
                        (prepareInput "SazE")
                        (Array.fromList
                            [ Array.fromList
                                [ { cellType = Start, elevation = 0, x = 0, y = 0 }
                                , { cellType = Journey, elevation = 0, x = 1, y = 0 }
                                , { cellType = Journey, elevation = 25, x = 2, y = 0 }
                                , { cellType = End, elevation = 25, x = 3, y = 0 }
                                ]
                            ]
                        )
            , test "charToCode a" <|
                \_ ->
                    Expect.equal
                        (charToCode 'a')
                        0
            , test "charToCode b" <|
                \_ ->
                    Expect.equal
                        (charToCode 'b')
                        1
            , test "charToCode z" <|
                \_ ->
                    Expect.equal
                        (charToCode 'z')
                        25
            , test "charToCode S" <|
                \_ ->
                    Expect.equal
                        (charToCode 'S')
                        startElevation
            , test "charToCode E" <|
                \_ ->
                    Expect.equal
                        (charToCode 'E')
                        endElevation
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
                        (nodeTraversable { elevation = 1, cellType = Journey, x = 0, y = 0 } { elevation = 1, cellType = Journey, x = 0, y = 0 })
                        True
            , test "nodeTraversable 1 2 True" <|
                \_ ->
                    Expect.equal
                        (nodeTraversable { elevation = 1, cellType = Journey, x = 0, y = 0 } { elevation = 2, cellType = Journey, x = 0, y = 0 })
                        True
            , test "nodeTraversable 1 3 False" <|
                \_ ->
                    Expect.equal
                        (nodeTraversable { elevation = 1, cellType = Journey, x = 0, y = 0 } { elevation = 3, cellType = Journey, x = 0, y = 0 })
                        False
            , test "nodeTraversable 5 3 False" <|
                \_ ->
                    Expect.equal
                        (nodeTraversable { elevation = 5, cellType = Journey, x = 0, y = 0 } { elevation = 3, cellType = Journey, x = 0, y = 0 })
                        False
            , test "nodeTraversable 5 4 True" <|
                \_ ->
                    Expect.equal
                        (nodeTraversable { elevation = 5, cellType = Journey, x = 0, y = 0 } { elevation = 4, cellType = Journey, x = 0, y = 0 })
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
            , test "twoDArrayToGraph testTwoDArray" <|
                \_ ->
                    Expect.equal
                        (twoDArrayToGraph (prepareInput "aSa\nbEb"))
                        (Dict.fromList
                            [ ( "0-0", { destination = Journey, key = "0-0", neighbours = [ "0-1", "1-0" ] } )
                            , ( "0-1", { destination = Journey, key = "0-1", neighbours = [ "0-0" ] } )
                            , ( "1-0", { destination = Start, key = "1-0", neighbours = [ "0-0", "2-0" ] } )
                            , ( "1-1", { destination = End, key = "1-1", neighbours = [] } )
                            , ( "2-0", { destination = Journey, key = "2-0", neighbours = [ "2-1", "1-0" ] } )
                            , ( "2-1", { destination = Journey, key = "2-1", neighbours = [ "2-0" ] } )
                            ]
                        )
            , test "twoDArrayToGraph simple" <|
                \_ ->
                    Expect.equal
                        (twoDArrayToGraph (prepareInput "abc\nbcd\ncde"))
                        (Dict.fromList
                            [ ( "0-0", { destination = Journey, key = "0-0", neighbours = [ "0-1", "1-0" ] } )
                            , ( "0-1", { destination = Journey, key = "0-1", neighbours = [ "0-0", "0-2", "1-1" ] } )
                            , ( "0-2", { destination = Journey, key = "0-2", neighbours = [ "0-1", "1-2" ] } )
                            , ( "1-0", { destination = Journey, key = "1-0", neighbours = [ "1-1", "0-0", "2-0" ] } )
                            , ( "1-1", { destination = Journey, key = "1-1", neighbours = [ "1-0", "1-2", "0-1", "2-1" ] } )
                            , ( "1-2", { destination = Journey, key = "1-2", neighbours = [ "1-1", "0-2", "2-2" ] } )
                            , ( "2-0", { destination = Journey, key = "2-0", neighbours = [ "2-1", "1-0" ] } )
                            , ( "2-1", { destination = Journey, key = "2-1", neighbours = [ "2-0", "2-2", "1-1" ] } )
                            , ( "2-2", { destination = Journey, key = "2-2", neighbours = [ "2-1", "1-2" ] } )
                            ]
                        )
            , test "findStart testInput" <|
                \_ ->
                    Expect.equal
                        (testInput
                            |> prepareInput
                            |> twoDArrayToGraph
                            |> findStart
                        )
                        (Just { destination = Start, key = "0-0", neighbours = [ "0-1", "1-0" ] })
            , test "findStart aSa\nbEb" <|
                \_ ->
                    Expect.equal
                        ("aSa\nbEb"
                            |> prepareInput
                            |> twoDArrayToGraph
                            |> findStart
                        )
                        (Just { destination = Start, key = "1-0", neighbours = [ "0-0", "2-0" ] })
            , test "removeNonUniqueValues" <|
                \_ ->
                    Expect.equal
                        (removeNonUniqueValues [ "a", "b", "c" ] [ "b", "d", "e" ])
                        [ "a", "c" ]
            , test "removeNonUniqueValues Long" <|
                \_ ->
                    Expect.equal
                        (removeNonUniqueValues [ "a", "b", "c", "d", "e", "f", "g" ] [ "b", "d", "f" ])
                        [ "a", "c", "e", "g" ]

            -- , test "part1Solution" <|
            --     \_ ->
            --         Expect.equal
            --             (part1Solution testInput)
            --             31
            , test "getNodesIfTravesable" <|
                \_ ->
                    let
                        cell =
                            Cell 1 Start 1 1
                    in
                    Expect.equal
                        (getNodesIfTravesable cell testCellMap)
                        [ { cellType = Journey, elevation = 2, x = 1, y = 0 }, { cellType = Journey, elevation = 2, x = 0, y = 1 } ]
            , test "simple puzzle answer" <|
                \_ ->
                    Expect.equal
                        (part1Solution "SbcdefghijklmnopqrstuvwxyzE")
                        25
            ]
        ]
