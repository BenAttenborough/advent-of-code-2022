module Advent9bTest exposing (..)

import Advent9Data exposing (partTwoBenInput, partTwoLargeInput, testInput)
import Advent9b exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html.Attributes exposing (list)
import List.Extra
import Parser exposing (Parser)
import Set exposing (Set)
import Test exposing (..)


suite : Test
suite =
    describe "Advent Day 9"
        [ describe "Part 1" <|
            [ test "createNCommands creates 3 UP comands with appropriate input" <|
                \_ ->
                    Expect.equal
                        (createNCommands 3 Up [])
                        [ Up, Up, Up ]
            , test "createNCommands creates 1 UP comands with appropriate input" <|
                \_ ->
                    Expect.equal
                        (createNCommands 1 Up [])
                        [ Up ]
            , test "createNCommands creates 0 comands with 0 input" <|
                \_ ->
                    Expect.equal
                        (createNCommands 0 Up [])
                        []
            , test "makeRope 10 produces a list of size 10" <|
                \_ ->
                    Expect.equal
                        (makeRope 10 []
                            |> List.length
                        )
                        10
            , test "makeRope 3 produces a rope with three two zero elementss" <|
                \_ ->
                    Expect.equal
                        (makeRope 3 [])
                        [ ( 0, 0 ), ( 0, 0 ), ( 0, 0 ) ]

            -- , test "moveKnotRelative" <|
            --     \_ ->
            --         Expect.equal
            --             ( ( 1, 0 ), ( 0, 0 ) )
            --             (moveKnotRelative ( 0, 0 ) ( 1, 0 ))
            -- ,test "Move a simple rope" <|
            --         \_ ->
            --             let
            --                 rope =
            --                     [ (1,1), (0,0), (0,0) ]
            --             in
            -- , skip <|
            --     test "createNCommands creates 0 comands with negative input" <|
            --         \_ ->
            --             Expect.equal
            --                 (createNCommands -1 Up [])
            --                 []
            -- , skip <|
            --     test "commandParser Up 3 creates 3 UP comands" <|
            --         \_ ->
            --             Expect.equal
            --                 (Parser.run commandParser "U 3")
            --                 (Ok [ Up, Up, Up ])
            -- , skip <|
            --     test "apply up command to knot 0 0" <|
            --         \_ ->
            --             Expect.equal
            --                 ( 0, 1 )
            --                 (applyCommandToKnot Up ( 0, 0 ))
            -- , skip <|
            --     test "apply multiple commands to the knot" <|
            --         \_ ->
            --             Expect.equal
            --                 ( -1, 2 )
            --                 (List.foldl applyCommandToKnot ( 0, 0 ) [ Up, Right, Up, Left, Left ])
            -- , skip <|
            --     test "apply multiple commands to the knot 2" <|
            --         \_ ->
            --             Expect.equal
            --                 ( 0, 0 )
            --                 (List.foldl applyCommandToKnot ( 0, 0 ) [ Up, Down, Left, Right ])
            -- , skip <|
            --     test "Move offset head (1,1) up to (1,2) tail should move to (1 ,1) (behind head - it should not move up only, but also across)" <|
            --         \_ ->
            --             let
            --                 rope =
            --                     [ ( 1, 1 ), ( 0, 0 ) ]
            --             in
            --             Expect.equal
            --                 [ ( 1, 2 ), ( 1, 1 ) ]
            --                 (applyCommandToRope Up rope)
            -- , skip <|
            --     test "Move knot with transform" <|
            --         \_ ->
            --             Expect.equal
            --                 ( 1, 1 )
            --                 (moveKnot ( 0, 0 ) ( 1, 1 ))
            -- , skip <|
            --     test "apply command 'Right' to rope " <|
            --         \_ ->
            --             let
            --                 rope =
            --                     [ ( 5, 0 ), ( 4, 0 ), ( 4, 0 ) ]
            --             in
            --             Expect.equal
            --                 [ ( 6, 0 ), ( 5, 0 ), ( 4, 0 ) ]
            --                 (applyCommandToRope Right rope)
            -- , skip <|
            --     test "apply command 'Up' to rope " <|
            --         \_ ->
            --             let
            --                 rope =
            --                     [ ( 0, 5 ), ( 0, 4 ), ( 0, 4 ) ]
            --             in
            --             Expect.equal
            --                 [ ( 0, 6 ), ( 0, 5 ), ( 0, 4 ) ]
            --                 (applyCommandToRope Up rope)
            -- , skip <|
            --     test "makeRope 10 then fold" <|
            --         \_ ->
            --             Expect.equal
            --                 [ ( -1, 4 ), ( 0, 3 ), ( 1, 2 ), ( 0, 1 ), ( 0, 0 ), ( 0, 0 ), ( 0, 0 ), ( 0, 0 ), ( 0, 0 ), ( 0, 0 ) ]
            --                 ([ Up, Up, Up, Up, Right, Right, Right, Left, Left, Left, Left ]
            --                     |> List.foldl
            --                         (\command rope ->
            --                             applyCommandToRope command rope
            --                         )
            --                         (makeRope 10 [])
            --                 )
            -- , skip <|
            --     test "output" <|
            --         \_ ->
            --             Expect.equal
            --                 ( [], Set.fromList [] )
            --                 (output [ Up, Up ])
            -- , skip <|
            --     test "outputFromString" <|
            --         \_ ->
            --             Expect.equal
            --                 ( [], Set.fromList [] )
            --                 (outputFromString partTwoBenInput)
            ]
        ]
