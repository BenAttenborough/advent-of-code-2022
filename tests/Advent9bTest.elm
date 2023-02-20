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
            , test "commandToTransform" <|
                \_ ->
                    Expect.equal
                        (commandToTransform Up)
                        ( 0, 1 )
            , test "getX (50, 100) to yeild 50" <|
                \_ ->
                    Expect.equal
                        (getX ( 50, 100 ))
                        50
            , test "getY (50, 100) to yeild 100" <|
                \_ ->
                    Expect.equal
                        (getY ( 50, 100 ))
                        100
            , test "moveKnot Up (5, 0) to yeild (5,1)" <|
                \_ ->
                    Expect.equal
                        (moveKnot Up ( 5, 0 ))
                        ( 5, 1 )
            , test "moveKnot Left (1, 0) to yeild (0,0)" <|
                \_ ->
                    Expect.equal
                        (moveKnot Left ( 1, 0 ))
                        ( 0, 0 )
            , test "moveKnotRelativeToLast Up (1, 1) (0,0) to yeild (0,0)" <|
                \_ ->
                    Expect.equal
                        (moveKnotRelativeToLast Up ( 1, 1 ) ( 0, 0 ))
                        ( 0, 0 )
            , test "moveKnotRelativeToLast Up (1, 2) (0,0) to yeild (1,1)" <|
                \_ ->
                    Expect.equal
                        (moveKnotRelativeToLast Up ( 1, 2 ) ( 0, 0 ))
                        ( 1, 1 )
            , test "moveKnotRelativeToLast Up (0, 2) (0,0) to yeild (0,1)" <|
                \_ ->
                    Expect.equal
                        (moveKnotRelativeToLast Up ( 0, 2 ) ( 0, 0 ))
                        ( 0, 1 )
            , test "moveKnotRelativeToLast Up (0, -2) (0,0) to yeild (0,0)" <|
                \_ ->
                    Expect.equal
                        (moveKnotRelativeToLast Up ( 0, -2 ) ( 0, 0 ))
                        ( 0, 0 )
            , test "moveKnotRelativeToLast Up (0, -2) (0,-4) to yeild (0,-3)" <|
                \_ ->
                    Expect.equal
                        (moveKnotRelativeToLast Up ( 0, -2 ) ( 0, -4 ))
                        ( 0, -3 )
            , test "moveKnotRelativeToLast Right (1, 1) (0,0) to yeild (0,0)" <|
                \_ ->
                    Expect.equal
                        (moveKnotRelativeToLast Right ( 1, 1 ) ( 0, 0 ))
                        ( 0, 0 )
            , test "moveKnotRelativeToLast Right (2, 1) (0,0) to yeild (1,1)" <|
                \_ ->
                    Expect.equal
                        (moveKnotRelativeToLast Right ( 2, 1 ) ( 1, 1 ))
                        ( 1, 1 )
            , test "moveKnotRelativeToLast Right (2, 0) (0,0) to yeild (1,0)" <|
                \_ ->
                    Expect.equal
                        (moveKnotRelativeToLast Right ( 2, 0 ) ( 1, 0 ))
                        ( 1, 0 )
            , test "moveKnotRelativeToLast Right (-2, 0) (0,0) to yeild (0,0)" <|
                \_ ->
                    Expect.equal
                        (moveKnotRelativeToLast Right ( -2, 0 ) ( 0, 0 ))
                        ( 0, 0 )
            , test "moveKnotRelativeToLast Right (-2, 0) (-4,0) to yeild (-3,0)" <|
                \_ ->
                    Expect.equal
                        (moveKnotRelativeToLast Right ( -2, 0 ) ( -4, 0 ))
                        ( -3, 0 )
            , test "moveKnotRelativeToLast Down (-1, -1) (0,0) to yeild (-1,-1)" <|
                \_ ->
                    Expect.equal
                        (moveKnotRelativeToLast Down ( -1, -1 ) ( 0, 0 ))
                        ( 0, 0 )
            , test "moveKnotRelativeToLast Down (-1, -2) (0,0) to yeild (-1,-1)" <|
                \_ ->
                    Expect.equal
                        (moveKnotRelativeToLast Down ( -1, -2 ) ( 0, 0 ))
                        ( -1, -1 )
            , test "moveKnotRelativeToLast Down (0, -2) (0,0) to yeild (0,-1)" <|
                \_ ->
                    Expect.equal
                        (moveKnotRelativeToLast Down ( 0, -2 ) ( 0, 0 ))
                        ( 0, -1 )
            , test "moveKnotRelativeToLast Down (0, 2) (0,4) to yeild (0,3)" <|
                \_ ->
                    Expect.equal
                        (moveKnotRelativeToLast Down ( 0, 2 ) ( 0, 4 ))
                        ( 0, 3 )
            , test "moveKnotRelativeToLast Left (-1, -1) (0,0) to yeild (0,0)" <|
                \_ ->
                    Expect.equal
                        (moveKnotRelativeToLast Left ( -1, -1 ) ( 0, 0 ))
                        ( 0, 0 )
            , test "moveKnotRelativeToLast Left (-2, -1) (0,0) to yeild (-1,-1)" <|
                \_ ->
                    Expect.equal
                        (moveKnotRelativeToLast Left ( -2, -1 ) ( 0, 0 ))
                        ( -1, -1 )
            , test "moveKnotRelativeToLast Left (2, 0) (4,0) to yeild (3,0)" <|
                \_ ->
                    Expect.equal
                        (moveKnotRelativeToLast Left ( 2, 0 ) ( 4, 0 ))
                        ( 3, 0 )
            , test "applyCommandToRope [(0,1), (0,0)] yeilds [(0,2), (0,1)]" <|
                \_ ->
                    Expect.equal
                        (applyCommandToRope Up [ ( 0, 1 ), ( 0, 0 ) ])
                        [ ( 0, 2 ), ( 0, 1 ) ]

            -- , test "applyCommandsToRope (makeRope 10 []) testInput" <|
            --     \_ ->
            --         Expect.equal
            --             (applyCommandsToRope initRopeState (parseCommandsFromInput testInput))
            --             (RopeState [] Set.empty)
            , test "applyCommandsToRope (makeRope 10 []) partTwoLargeInput" <|
                \_ ->
                    Expect.equal
                        (applyCommandsToRope initRopeState (parseCommandsFromInput partTwoLargeInput))
                        (RopeState [] [])
            , test "main init testInput" <|
                \_ ->
                    Expect.equal
                        (applyCommandsToRope initRopeState (parseCommandsFromInput testInput)
                            |> (\ropeState -> ropeState.visited |> List.length)
                        )
                        1
            , test "main init partTwoLargeInput" <|
                \_ ->
                    Expect.equal
                        (applyCommandsToRope initRopeState (parseCommandsFromInput partTwoLargeInput)
                            |> (\ropeState -> ropeState.visited |> List.length)
                        )
                        36
            ]
        ]
