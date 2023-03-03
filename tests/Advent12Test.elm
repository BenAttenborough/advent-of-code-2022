module Advent12Test exposing (..)

import Advent12 exposing (..)
import Array exposing (Array)
import Data.Advent12Data exposing (testInput)
import Expect
import Test exposing (..)


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
            ]
        ]
