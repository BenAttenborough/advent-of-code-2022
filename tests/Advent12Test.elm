module Advent12Test exposing (..)

-- import Dict
-- import Data.Advent12Data exposing (difficultInput, pruningInput, realInput, simplifiedInput, testInput)

import Advent12 exposing (..)
import Array exposing (Array)
import Dict
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
                (findStart "aaaa\nbSbb\ncccc")
                (Just ( 1, 1, 0 ))
    , test "simpleMaze no start" <|
        \_ ->
            Expect.equal
                (findStart "aaaa\nbbbb\ncccc")
                Nothing
    , test "simpleMaze more than 1 start" <|
        \_ ->
            Expect.equal
                (findStart "aSaa\nbbbb\ncScc")
                Nothing
    ]


findEndTests : List Test
findEndTests =
    [ test "simpleMaze" <|
        \_ ->
            Expect.equal
                (findEnd "aaaa\nbEbb\ncccc")
                (Just ( 1, 1, 25 ))
    , test "simpleMaze end at end" <|
        \_ ->
            Expect.equal
                (findEnd "aaaa\nbbbb\ncccE")
                (Just ( 3, 2, 25 ))
    , test "simpleMaze no end" <|
        \_ ->
            Expect.equal
                (findEnd "aaaa\nbbbb\ncccc")
                Nothing
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


inputToAtlasTests : List Test
inputToAtlasTests =
    [ test "2d array" <|
        \_ ->
            Expect.equal
                (inputToAtlas "bcde")
                (Array.fromList
                    [ Array.fromList
                        [ ( 0, 0, 1 ), ( 1, 0, 2 ), ( 2, 0, 3 ), ( 3, 0, 4 ) ]
                    ]
                )
    , test "2d array with S and E chars (tests this are converted to lowercase first)" <|
        \_ ->
            Expect.equal
                (inputToAtlas "SbczE")
                (Array.fromList
                    [ Array.fromList
                        [ ( 0, 0, 0 ), ( 1, 0, 1 ), ( 2, 0, 2 ), ( 3, 0, 25 ), ( 4, 0, 25 ) ]
                    ]
                )
    , test "Simple array" <|
        \_ ->
            Expect.equal
                (inputToAtlas "aaaa\nbbbb\ncccc")
                (Array.fromList
                    -- []
                    [ Array.fromList [ ( 0, 0, 0 ), ( 1, 0, 0 ), ( 2, 0, 0 ), ( 3, 0, 0 ) ]
                    , Array.fromList [ ( 0, 1, 1 ), ( 1, 1, 1 ), ( 2, 1, 1 ), ( 3, 1, 1 ) ]
                    , Array.fromList [ ( 0, 2, 2 ), ( 1, 2, 2 ), ( 2, 2, 2 ), ( 3, 2, 2 ) ]
                    ]
                )

    --             (findEnd "aaaa\nbbbb\ncccE")
    --             (Just ( 3, 2, 25 ))
    , test "End test" <|
        \_ ->
            Expect.equal
                (inputToAtlas "aaaa\nbbbb\ncccE")
                (Array.fromList
                    [ Array.fromList [ ( 0, 0, 0 ), ( 1, 0, 0 ), ( 2, 0, 0 ), ( 3, 0, 0 ) ]
                    , Array.fromList [ ( 0, 1, 1 ), ( 1, 1, 1 ), ( 2, 1, 1 ), ( 3, 1, 1 ) ]
                    , Array.fromList [ ( 0, 2, 2 ), ( 1, 2, 2 ), ( 2, 2, 2 ), ( 3, 2, 25 ) ]
                    ]
                )
    ]


array2dToDictTests : List Test
array2dToDictTests =
    [ test "simple input" <|
        \_ ->
            Expect.equal 1 1

    -- (inputToAtlas "aaaa\nbbbb\ncccE"
    --     |> array2dToDict
    -- )
    -- Dict.toList
    ]


baseTests : List Test
baseTests =
    [ describe "charToCodeTests" <| charToCodeTests
    , describe "inputToCharArray" <| inputToAtlasTests
    , describe "findStartTests" <| findStartTests
    , describe "findEndTests" <| findEndTests
    , describe "array2dToDictTests" <| array2dToDictTests
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
