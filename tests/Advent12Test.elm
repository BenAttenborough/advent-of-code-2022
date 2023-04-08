module Advent12Test exposing (..)

-- import Dict
-- import Data.Advent12Data exposing (difficultInput, pruningInput, realInput, simplifiedInput, testInput)

import Advent12 exposing (..)
import Array exposing (Array)
import Expect
import Test exposing (..)


simpleMaze : Array (Array ( Int, Int, Int ))
simpleMaze =
    Array.fromList
        [ Array.fromList [ ( 0, 0, 0 ), ( 1, 0, 0 ), ( 2, 0, 0 ) ]
        , Array.fromList [ ( 0, 1, 1 ), ( 1, 1, 22 ), ( 2, 1, 1 ) ]
        , Array.fromList [ ( 0, 2, 20 ), ( 1, 2, 3 ), ( 2, 2, 3 ) ]
        ]



-- test "S" <|
--         \_ ->
--             Expect.equal
--                 ()
--                 ()


findStartTests : List Test
findStartTests =
    [ test "simpleMaze" <|
        \_ ->
            Expect.equal
                (findStart simpleMaze)
                (Just ( 1, 1 ))
    ]


findEndTests : List Test
findEndTests =
    [ test "simpleMaze" <|
        \_ ->
            Expect.equal
                (findEnd simpleMaze)
                (Just ( 0, 2 ))
    ]


charToCodeTests : List Test
charToCodeTests =
    [ test "S" <|
        \_ ->
            Expect.equal
                (charToCode 'S')
                0
    , test "E" <|
        \_ ->
            Expect.equal
                (charToCode 'E')
                25
    , test "a" <|
        \_ ->
            Expect.equal
                (charToCode 'a')
                0
    , test "z" <|
        \_ ->
            Expect.equal
                (charToCode 'z')
                25
    ]


inputToCharArrayTests : List Test
inputToCharArrayTests =
    [ test "Simple array" <|
        \_ ->
            Expect.equal
                (inputToAtlas "aaa\nbbb\nccc")
                (Array.fromList
                    [ Array.fromList [ ( 0, 0, 0 ), ( 1, 0, 0 ), ( 2, 0, 0 ) ]
                    , Array.fromList [ ( 0, 1, 1 ), ( 1, 1, 1 ), ( 2, 1, 1 ) ]
                    , Array.fromList [ ( 0, 2, 2 ), ( 1, 2, 2 ), ( 2, 2, 2 ) ]
                    ]
                )
    ]


baseTests : List Test
baseTests =
    [ describe "charToCodeTests" <| charToCodeTests
    , describe "inputToCharArray" <| inputToCharArrayTests
    , describe "findStartTests" <| findStartTests
    , describe "findEndTests" <| findEndTests
    ]


devTests : List Test
devTests =
    [ test "foo" <|
        \_ -> Expect.equal 1 1
    ]


solutionsTests : List Test
solutionsTests =
    [ test "null" <|
        \_ ->
            Expect.equal
                1
                1
    ]


suite : Test
suite =
    describe "Advent12"
        [ describe "Part 1" <|
            [ describe "Base tests" <| baseTests
            , describe "Dev tests" <| devTests
            , describe "Test solutions" <| solutionsTests
            ]
        ]
