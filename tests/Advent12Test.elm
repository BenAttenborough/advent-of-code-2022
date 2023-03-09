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
                        (Just { elevation = 1, cellType = Journey, x = 0, y = 0 })
            , test "nodeTraversable 1 2 True" <|
                \_ ->
                    Expect.equal
                        (nodeTraversable { elevation = 1, cellType = Journey, x = 0, y = 0 } { elevation = 2, cellType = Journey, x = 0, y = 0 })
                        (Just { elevation = 2, cellType = Journey, x = 0, y = 0 })
            , test "nodeTraversable 1 3 False" <|
                \_ ->
                    Expect.equal
                        (nodeTraversable { elevation = 1, cellType = Journey, x = 0, y = 0 } { elevation = 3, cellType = Journey, x = 0, y = 0 })
                        Nothing
            , test "nodeTraversable 5 3 False" <|
                \_ ->
                    Expect.equal
                        (nodeTraversable { elevation = 5, cellType = Journey, x = 0, y = 0 } { elevation = 3, cellType = Journey, x = 0, y = 0 })
                        Nothing
            , test "nodeTraversable 5 4 True" <|
                \_ ->
                    Expect.equal
                        (nodeTraversable { elevation = 5, cellType = Journey, x = 0, y = 0 } { elevation = 4, cellType = Journey, x = 0, y = 0 })
                        (Just { elevation = 4, cellType = Journey, x = 0, y = 0 })
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

            -- , test "twoDArrayToGraph testTwoDArray" <|
            --     \_ ->
            --         Expect.equal
            --             (cellArrayToCellGraph (prepareInput "aSa\nbEb"))
            --             (Dict.fromList
            --                 [ ( "0-0", { destination = Journey, key = "0-0", neighbours = [ "0-1", "1-0" ] } )
            --                 , ( "0-1", { destination = Journey, key = "0-1", neighbours = [ "0-0" ] } )
            --                 , ( "1-0", { destination = Start, key = "1-0", neighbours = [ "0-0", "2-0" ] } )
            --                 , ( "1-1", { destination = End, key = "1-1", neighbours = [] } )
            --                 , ( "2-0", { destination = Journey, key = "2-0", neighbours = [ "2-1", "1-0" ] } )
            --                 , ( "2-1", { destination = Journey, key = "2-1", neighbours = [ "2-0" ] } )
            --                 ]
            --             )
            -- , test "twoDArrayToGraph simple" <|
            --     \_ ->
            --         Expect.equal
            --             (cellArrayToCellGraph (prepareInput "abc\nbcd\ncde"))
            --             (Dict.fromList
            --                 [ ( "0-0", { destination = Journey, key = "0-0", neighbours = [ "0-1", "1-0" ] } )
            --                 , ( "0-1", { destination = Journey, key = "0-1", neighbours = [ "0-2", "1-1" ] } )
            --                 , ( "0-2", { destination = Journey, key = "0-2", neighbours = [ "1-2" ] } )
            --                 , ( "1-0", { destination = Journey, key = "1-0", neighbours = [ "1-1", "2-0" ] } )
            --                 , ( "1-1", { destination = Journey, key = "1-1", neighbours = [ "1-2", "2-1" ] } )
            --                 , ( "1-2", { destination = Journey, key = "1-2", neighbours = [ "2-2" ] } )
            --                 , ( "2-0", { destination = Journey, key = "2-0", neighbours = [ "2-1" ] } )
            --                 , ( "2-1", { destination = Journey, key = "2-1", neighbours = [ "2-2" ] } )
            --                 , ( "2-2", { destination = Journey, key = "2-2", neighbours = [] } )
            --                 ]
            --             )
            -- , test "findStart testInput" <|
            --     \_ ->
            --         Expect.equal
            --             (testInput
            --                 |> prepareInput
            --                 |> cellArrayToCellGraph
            --                 |> findStart
            --             )
            --             (Just { destination = Start, key = "0-0", neighbours = [ "0-1", "1-0" ] })
            -- , test "findStart aSa\nbEb" <|
            --     \_ ->
            --         Expect.equal
            --             ("aSa\nbEb"
            --                 |> prepareInput
            --                 |> cellArrayToCellGraph
            --                 |> findStart
            --             )
            --             (Just { destination = Start, key = "1-0", neighbours = [ "0-0", "2-0" ] })
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

            -- , test "cellListToNeighboursList" <|
            --     \_ ->
            --         let
            --             arr =
            --                 Array.fromList
            --                     [ Array.fromList
            --                         [ Cell 1 Journey 0 0
            --                         , Cell 1 Journey 1 0
            --                         , Cell 1 Journey 2 0
            --                         , Cell 1 Journey 3 0
            --                         , Cell 1 Journey 4 0
            --                         ]
            --                     ]
            --             listCell =
            --                 arr
            --                     |> Array.map Array.toList
            --                     |> Array.toList
            --                     |> List.concat
            --         in
            --         Expect.equal
            --             (cellListToNeighboursList
            --                 arr
            --                 []
            --                 []
            --                 listCell
            --             )
            --             [ ( "0-0", { destination = Journey, key = "0-0", neighbours = [ "1-0" ] } )
            --             , ( "1-0", { destination = Journey, key = "1-0", neighbours = [ "2-0" ] } )
            --             , ( "2-0", { destination = Journey, key = "2-0", neighbours = [ "3-0" ] } )
            --             , ( "3-0", { destination = Journey, key = "3-0", neighbours = [ "4-0" ] } )
            --             , ( "4-0", { destination = Journey, key = "4-0", neighbours = [] } )
            --             ]
            ]
        ]


help : Dict.Dict String GraphNode
help =
    Dict.fromList
        [ ( "0-0", { destination = Start, key = "0-0", neighbours = [ "1-0" ] } )
        , ( "1-0", { destination = Journey, key = "1-0", neighbours = [ "2-0" ] } )
        , ( "10-0", { destination = Journey, key = "10-0", neighbours = [ "11-0" ] } )
        , ( "11-0", { destination = Journey, key = "11-0", neighbours = [ "12-0" ] } )
        , ( "12-0", { destination = Journey, key = "12-0", neighbours = [ "13-0" ] } )
        , ( "13-0", { destination = Journey, key = "13-0", neighbours = [ "14-0" ] } )
        , ( "14-0", { destination = Journey, key = "14-0", neighbours = [ "15-0" ] } )
        , ( "15-0", { destination = Journey, key = "15-0", neighbours = [ "16-0" ] } )
        , ( "16-0", { destination = Journey, key = "16-0", neighbours = [ "17-0" ] } )
        , ( "17-0", { destination = Journey, key = "17-0", neighbours = [ "18-0" ] } )
        , ( "18-0", { destination = Journey, key = "18-0", neighbours = [ "19-0" ] } )
        , ( "19-0", { destination = Journey, key = "19-0", neighbours = [ "20-0" ] } )
        , ( "2-0", { destination = Journey, key = "2-0", neighbours = [ "3-0" ] } )
        , ( "20-0", { destination = Journey, key = "20-0", neighbours = [ "21-0" ] } )
        , ( "21-0", { destination = Journey, key = "21-0", neighbours = [ "22-0" ] } )
        , ( "22-0", { destination = Journey, key = "22-0", neighbours = [ "23-0" ] } )
        , ( "23-0", { destination = Journey, key = "23-0", neighbours = [ "24-0" ] } )
        , ( "24-0", { destination = Journey, key = "24-0", neighbours = [ "25-0" ] } )
        , ( "25-0", { destination = Journey, key = "25-0", neighbours = [ "26-0" ] } )
        , ( "26-0", { destination = End, key = "26-0", neighbours = [] } )
        , ( "3-0", { destination = Journey, key = "3-0", neighbours = [ "4-0" ] } )
        , ( "4-0", { destination = Journey, key = "4-0", neighbours = [ "5-0" ] } )
        , ( "5-0", { destination = Journey, key = "5-0", neighbours = [ "6-0" ] } )
        , ( "6-0", { destination = Journey, key = "6-0", neighbours = [ "7-0" ] } )
        , ( "7-0", { destination = Journey, key = "7-0", neighbours = [ "8-0" ] } )
        , ( "8-0", { destination = Journey, key = "8-0", neighbours = [ "9-0" ] } )
        , ( "9-0", { destination = Journey, key = "9-0", neighbours = [ "10-0" ] } )
        ]


help2 =
    Dict.fromList
        [ ( "00000-00000", { destination = Start, key = "00000-00000", neighbours = [ "00001-00000" ] } )
        , ( "00001-00000", { destination = Journey, key = "00001-00000", neighbours = [ "00002-00000" ] } )
        , ( "00002-00000", { destination = Journey, key = "00002-00000", neighbours = [ "00003-00000" ] } )
        , ( "00003-00000", { destination = Journey, key = "00003-00000", neighbours = [ "00004-00000" ] } )
        , ( "00004-00000", { destination = Journey, key = "00004-00000", neighbours = [ "00005-00000" ] } )
        , ( "00005-00000", { destination = Journey, key = "00005-00000", neighbours = [ "00006-00000" ] } )
        , ( "00006-00000", { destination = Journey, key = "00006-00000", neighbours = [ "00007-00000" ] } )
        , ( "00007-00000", { destination = Journey, key = "00007-00000", neighbours = [ "00008-00000" ] } )
        , ( "00008-00000", { destination = Journey, key = "00008-00000", neighbours = [ "00009-00000" ] } )
        , ( "00009-00000", { destination = Journey, key = "00009-00000", neighbours = [ "00010-00000" ] } )
        , ( "00010-00000", { destination = Journey, key = "00010-00000", neighbours = [ "00011-00000" ] } )
        , ( "00011-00000", { destination = Journey, key = "00011-00000", neighbours = [ "00012-00000" ] } )
        , ( "00012-00000", { destination = Journey, key = "00012-00000", neighbours = [ "00013-00000" ] } )
        , ( "00013-00000", { destination = Journey, key = "00013-00000", neighbours = [ "00014-00000" ] } )
        , ( "00014-00000", { destination = Journey, key = "00014-00000", neighbours = [ "00015-00000" ] } )
        , ( "00015-00000", { destination = Journey, key = "00015-00000", neighbours = [ "00016-00000" ] } )
        , ( "00016-00000", { destination = Journey, key = "00016-00000", neighbours = [ "00017-00000" ] } )
        , ( "00017-00000", { destination = Journey, key = "00017-00000", neighbours = [ "00018-00000" ] } )
        , ( "00018-00000", { destination = Journey, key = "00018-00000", neighbours = [ "00019-00000" ] } )
        , ( "00019-00000", { destination = Journey, key = "00019-00000", neighbours = [ "00020-00000" ] } )
        , ( "00020-00000", { destination = Journey, key = "00020-00000", neighbours = [ "00021-00000" ] } )
        , ( "00021-00000", { destination = Journey, key = "00021-00000", neighbours = [ "00022-00000" ] } )
        , ( "00022-00000", { destination = Journey, key = "00022-00000", neighbours = [ "00023-00000" ] } )
        , ( "00023-00000", { destination = Journey, key = "00023-00000", neighbours = [ "00024-00000" ] } )
        , ( "00024-00000", { destination = Journey, key = "00024-00000", neighbours = [ "00025-00000" ] } )
        , ( "00025-00000"
          , { destination = Journey, key = "00025-00000", neighbours = [ "00026-00000" ] }
          )
        , ( "00026-00000", { destination = End, key = "00026-00000", neighbours = [] } )
        ]
