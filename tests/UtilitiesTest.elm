module UtilitiesTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser exposing (..)
import Test exposing (..)
import Utilities.Utilities exposing (partitioner)


suite : Test
suite =
    describe "Utilities"
        [ describe "partitioner" <|
            [ test "splits list into chunks of 2" <|
                \_ ->
                    Expect.equal
                        (partitioner 2 [] [ 1, 2, 3, 4, 5, 6 ])
                        [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ] ]
            , test "Works with odd length" <|
                \_ ->
                    Expect.equal
                        (partitioner 2 [] [ 1, 2, 3, 4, 5 ])
                        [ [ 1, 2 ], [ 3, 4 ], [ 5 ] ]
            , test "Works with size too big" <|
                \_ ->
                    Expect.equal
                        (partitioner 50 [] [ 1, 2, 3, 4, 5 ])
                        [ [ 1, 2, 3, 4, 5 ] ]
            , test "Works with size 0" <|
                \_ ->
                    Expect.equal
                        (partitioner 0 [] [ 1, 2, 3, 4, 5 ])
                        [ [ 1, 2, 3, 4, 5 ] ]
            , test "Works with negative size" <|
                \_ ->
                    Expect.equal
                        (partitioner -1 [] [ 1, 2, 3, 4, 5 ])
                        [ [ 1, 2, 3, 4, 5 ] ]
            ]
        ]
