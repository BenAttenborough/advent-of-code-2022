module UtilitiesTest exposing (..)

import Array exposing (Array)
import Expect
import Parser exposing (..)
import Test exposing (..)
import Utilities.Utilities exposing (build2DArray, getElementFrom2DArray, partitioner, uniqueItemFrom2DArray)


testTwoDArray : Array (Array Int)
testTwoDArray =
    Array.fromList
        [ Array.fromList [ 0, 1, 2 ]
        , Array.fromList [ 3, 4, 5 ]
        , Array.fromList [ 6, 7, 8 ]
        ]


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
        , describe "getElementFrom2DArray" <|
            [ test "getElementFrom2DArray 1 1 testTwoDArray = Just 4" <|
                \_ ->
                    Expect.equal
                        (getElementFrom2DArray 1 1 testTwoDArray)
                        (Just 4)
            , test "getElementFrom2DArray 0 0 testTwoDArray = Just 0" <|
                \_ ->
                    Expect.equal
                        (getElementFrom2DArray 0 0 testTwoDArray)
                        (Just 0)
            , test "getElementFrom2DArray 3 0 testTwoDArray = Nothing" <|
                \_ ->
                    Expect.equal
                        (getElementFrom2DArray 3 0 testTwoDArray)
                        Nothing
            , test "getElementFrom2DArray -1 -1 testTwoDArray = Nothing" <|
                \_ ->
                    Expect.equal
                        (getElementFrom2DArray -1 -1 testTwoDArray)
                        Nothing
            , describe "build2DArray" <|
                [ test "test" <|
                    \_ ->
                        Expect.equal
                            (build2DArray
                                [ [ 'a', 'b', 'c' ]
                                , [ 'd', 'e', 'f' ]
                                , [ 'g', 'h', 'i' ]
                                ]
                            )
                            (Array.fromList
                                [ Array.fromList [ 'a', 'b', 'c' ]
                                , Array.fromList [ 'd', 'e', 'f' ]
                                , Array.fromList [ 'g', 'h', 'i' ]
                                ]
                            )
                ]
            , describe "uniqueItemFrom2DArray" <|
                [ test "test" <|
                    let
                        testArray =
                            build2DArray
                                [ [ 'a', 'b', 'c' ]
                                , [ 'd', 'e', 'f' ]
                                , [ 'g', 'h', 'i' ]
                                ]

                        predicate =
                            \a -> a == 'e'
                    in
                    \_ ->
                        Expect.equal
                            (uniqueItemFrom2DArray predicate testArray)
                            (Just 'e')
                , test "test not unique" <|
                    let
                        testArray =
                            build2DArray
                                [ [ 'a', 'b', 'c' ]
                                , [ 'd', 'e', 'f' ]
                                , [ 'g', 'h', 'e' ]
                                ]

                        predicate =
                            \a -> a == 'e'
                    in
                    \_ ->
                        Expect.equal
                            (uniqueItemFrom2DArray predicate testArray)
                            Nothing
                ]
            ]
        ]
