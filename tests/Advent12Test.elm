module Advent12Test exposing (..)

import Advent12 exposing (..)
import Array exposing (Array)
import Data.Advent12Data exposing (realInput, testInput)
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


getNodeTests : List Test
getNodeTests =
    [ test "getNode 1 1 testTwoDArray = Just 4" <|
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


twoDArrayToGraphTests : List Test
twoDArrayToGraphTests =
    [ test "twoDArrayToGraph testTwoDArray" <|
        \_ ->
            Expect.equal
                (cellArrayToCellGraph (prepareInput "aSa\nbEb"))
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
                (cellArrayToCellGraph (prepareInput "abc\nbcd\ncde"))
                (Dict.fromList
                    [ ( "0-0", { destination = Journey, key = "0-0", neighbours = [ "0-1", "1-0" ] } )
                    , ( "0-1", { destination = Journey, key = "0-1", neighbours = [ "0-2", "1-1" ] } )
                    , ( "0-2", { destination = Journey, key = "0-2", neighbours = [ "1-2" ] } )
                    , ( "1-0", { destination = Journey, key = "1-0", neighbours = [ "1-1", "2-0" ] } )
                    , ( "1-1", { destination = Journey, key = "1-1", neighbours = [ "1-2", "2-1" ] } )
                    , ( "1-2", { destination = Journey, key = "1-2", neighbours = [ "2-2" ] } )
                    , ( "2-0", { destination = Journey, key = "2-0", neighbours = [ "2-1" ] } )
                    , ( "2-1", { destination = Journey, key = "2-1", neighbours = [ "2-2" ] } )
                    , ( "2-2", { destination = Journey, key = "2-2", neighbours = [] } )
                    ]
                )
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


removeNonUniqueValuesTests : List Test
removeNonUniqueValuesTests =
    [ test "removeNonUniqueValues simple" <|
        \_ ->
            Expect.equal
                (removeNonUniqueValues [ "a", "b", "c" ] [ "b", "d", "e" ])
                [ "a", "c" ]
    , test "removeNonUniqueValues Long" <|
        \_ ->
            Expect.equal
                (removeNonUniqueValues [ "a", "b", "c", "d", "e", "f", "g" ] [ "b", "d", "f" ])
                [ "a", "c", "e", "g" ]
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
    [ -- test "simple puzzle answer" <|
      -- \_ ->
      --     Expect.equal
      --         (part1Solution "SabcdefghijklmnopqrstuvwxyzE")
      --         (Just 27)
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
      -- ]
      --   test "part1Solution" <|
      --     \_ ->
      --         Expect.equal
      --             (part1Solution realInput)
      --             (Just 31)
      test "part1Solution testInput" <|
        \_ ->
            Expect.equal
                (part1Solution realInput)
                -- (Just 31)
                (Just { cellType = End, elevation = 25, x = 5, y = 2 })
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
    , describe "getNode / nodes" <| getNodeTests
    , describe "nodeTraversable" <| nodeTraversableTests
    , skip <| describe "twoDArrayToGraph" <| twoDArrayToGraphTests
    , describe "Find start" <| findStartTests
    , describe "removeNonUniqueValues" <| removeNonUniqueValuesTests
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
            , skip <| describe "Test solutions" <| solutionsTests
            ]
        ]
