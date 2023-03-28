module Advent12Test exposing (..)

import Advent12 exposing (..)
import Array exposing (Array)
import Data.Advent12Data exposing (difficultInput, pruningInput, realInput, simplifiedInput, testInput)
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


prepareInputTests : List Test
prepareInputTests =
    [ test "prepareInput" <|
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
    ]


charToCodeTests : List Test
charToCodeTests =
    [ test "charToCode a" <|
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
    ]


nodeTraversableTests : List Test
nodeTraversableTests =
    [ test "nodeTraversable 1 1 True" <|
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
    ]


findStartTests : List Test
findStartTests =
    [ test "findStart testInput" <|
        \_ ->
            Expect.equal
                (testInput
                    |> prepareInput
                    |> findStart
                )
                (Just { cellType = Start, elevation = 0, x = 0, y = 0 })
    , test "findStart aSa\nbEb" <|
        \_ ->
            Expect.equal
                ("aSa\nbEb"
                    |> prepareInput
                    |> findStart
                )
                (Just { cellType = Start, elevation = 0, x = 1, y = 0 })
    ]


getAvailableNeighboursTests : List Test
getAvailableNeighboursTests =
    [ test "getAvailableNeighbours 0 neighbours" <|
        let
            cell =
                Cell 0 Start 0 0

            atlas =
                Array.fromList [ Array.fromList [ cell ] ]
        in
        \_ ->
            Expect.equal
                (getAvailableNeighbours cell atlas "")
                []
    , test "getAvailableNeighbours 1 neighbour" <|
        let
            cell =
                Cell 0 Start 0 0

            atlas =
                Array.fromList
                    [ Array.fromList [ cell, Cell 0 Journey 1 0 ]
                    ]
        in
        \_ ->
            Expect.equal
                (getAvailableNeighbours cell atlas "")
                [ "01-00" ]
    , test "getAvailableNeighbours 2 neighbours" <|
        let
            cell =
                Cell 0 Start 1 0

            atlas =
                Array.fromList
                    [ Array.fromList [ Cell 0 Journey 0 0, cell, Cell 0 Journey 2 0 ]
                    ]
        in
        \_ ->
            Expect.equal
                (getAvailableNeighbours cell atlas "")
                [ "00-00", "02-00" ]
    , test "getAvailableNeighbours 2 neighbours but ignore parent" <|
        let
            cell =
                Cell 0 Start 1 0

            atlas =
                Array.fromList
                    [ Array.fromList [ Cell 0 Journey 0 0, cell, Cell 0 Journey 2 0 ]
                    ]
        in
        \_ ->
            Expect.equal
                (getAvailableNeighbours cell atlas "00-00")
                [ "02-00" ]
    ]


solutionsTests : List Test
solutionsTests =
    [ test "null" <|
        \_ ->
            Expect.equal
                1
                1

    -- , test "simple puzzle answer" <|
    --     \_ ->
    --         Expect.equal
    --             (part1Solution "SabcdefghijklmnopqrstuvwxyzE")
    --             (Just 27)
    -- , test "testInput" <|
    --     \_ ->
    --         Expect.equal
    --             (part1Solution testInput)
    --             (Just 31)
    , test "simplifiedInput" <|
        \_ ->
            Expect.equal
                (part1Solution simplifiedInput)
                (Just 68)

    -- simplifiedInput
    -- , test "part1Solution realInput" <|
    --     \_ ->
    --         Expect.equal
    --             (part1Solution realInput)
    --             (Just 10)
    -- , test "simple puzzle answer" <|
    --     \_ ->
    --         Expect.equal
    --             (part1Solution "SabcdefghijklmnopqrstuvwxyzE")
    --             (Just { cellType = End, elevation = 25, x = 27, y = 0 })
    --   ,only <|
    --     test "very simple puzzle answer" <|
    --         \_ ->
    --             Expect.equal
    --                 (part1Solution "SabcdefghijklmnopqrstuvwxyzE")
    --                 Nothing
    -- , test "pruning test" <|
    --     \_ ->
    --         Expect.equal
    --             (part1Solution pruningInput)
    --             Nothing
    -- , test "testInput" <|
    --     \_ ->
    --         Expect.equal
    --             (part1Solution testInput)
    --             (Just { cellType = End, elevation = 25, x = 5, y = 2 })
    -- , test "difficultInput" <|
    --     \_ ->
    --         Expect.equal
    --             (part1Solution difficultInput)
    --             Nothing
    -- , test "part1Solution realInput" <|
    --     \_ ->
    --         Expect.equal
    --             (part1Solution realInput)
    --             (Just { cellType = End, elevation = 25, x = 5, y = 2 })
    ]


getAvailableNeighboursCellTests : List Test
getAvailableNeighboursCellTests =
    [ test "getAvailableNeighboursCell 2 neighbours" <|
        let
            cell =
                Cell 0 Start 1 0

            atlas =
                Array.fromList
                    [ Array.fromList [ Cell 0 Journey 0 0, cell, Cell 0 Journey 2 0 ]
                    ]
        in
        \_ ->
            Expect.equal
                (getAvailableNeighboursCell cell atlas)
                [ { cellType = Journey, elevation = 0, x = 0, y = 0 }
                , { cellType = Journey, elevation = 0, x = 2, y = 0 }
                ]
    ]


baseTests : List Test
baseTests =
    [ describe "Prepare Input" <| prepareInputTests
    , describe "charToCode" <| charToCodeTests
    , describe "nodeTraversable" <| nodeTraversableTests
    , describe "Find start" <| findStartTests
    , describe "getAvailableNeighbours" <| getAvailableNeighboursTests
    , describe "getAvailableNeighboursCell" <| getAvailableNeighboursCellTests
    ]


devTests : List Test
devTests =
    [ test "foo" <|
        \_ -> Expect.equal 1 1
    ]


suite : Test
suite =
    describe "Advent12"
        [ describe "Part 1" <|
            [ describe "Base tests" <| baseTests
            , describe "Dev tests" <| devTests
            , only <| describe "Test solutions" <| solutionsTests
            ]
        ]
