module Advent9Test exposing (..)

import Advent9 exposing (..)
import Advent9Data exposing (testInput)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser exposing (Parser)
import Set exposing (Set)
import Test exposing (..)


exampleCommand : String
exampleCommand =
    "U 10"


ropeStateZero : Rope
ropeStateZero =
    { tail = ( 0, 0 )
    , headRel = ( 0, 0 )
    , visited = Set.singleton ( 0, 0 )
    }


ropeStateUpFromZero : Rope
ropeStateUpFromZero =
    { tail = ( 0, 0 )
    , headRel = ( 0, 1 )
    , visited = Set.fromList [ ( 0, 0 ) ]
    }


suite : Test
suite =
    describe "Advent Day 9"
        [ describe "Part 1" <|
            [ -- test "Expect commandParser exampleCommand to equal Up 10" <|
              --     \_ ->
              --         Expect.equal
              --             (Parser.run commandParser exampleCommand)
              --             (Ok (Up 10))
              -- , test "Parsing test input gives expected output" <|
              --     \_ ->
              --         Expect.equal
              --             (parseCommandsFromInput testInput)
              --             [ Right 4, Up 4, Left 3, Down 1, Right 4, Down 1, Left 5, Right 2 ]
              test "createNCommands creates 3 UP comands with appropriate input" <|
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
            , test "createNCommands creates 0 comands with negative input" <|
                \_ ->
                    Expect.equal
                        (createNCommands -1 Up [])
                        []
            , test "commandParser Up 3 creates 3 UP comands" <|
                \_ ->
                    Expect.equal
                        (Parser.run commandParser "U 3")
                        (Ok [ Up, Up, Up ])

            -- Up
            , test "applyCommandsToRopeState moves rope correctly up" <|
                \_ ->
                    Expect.equal
                        (applyCommandsToRopeState Up ropeStateZero)
                        ropeStateUpFromZero
            , test "applyCommandsToRopeState moves rope correctly up when head's rel pos moves beyond boundary" <|
                \_ ->
                    let
                        initialState =
                            { headRel = ( 0, 1 )
                            , tail = ( 0, 0 )
                            , visited = Set.singleton ( 0, 0 )
                            }

                        expectedState =
                            { headRel = ( 0, 1 )
                            , tail = ( 0, 1 )
                            , visited = Set.fromList [ ( 0, 0 ), ( 0, 1 ) ]
                            }
                    in
                    Expect.equal
                        (applyCommandsToRopeState Up initialState)
                        expectedState
            , test "applyCommandsToRopeState moves rope correctly up when head's rel pos moves beyond boundary and it is offset" <|
                \_ ->
                    let
                        initialState =
                            { headRel = ( 1, 1 )
                            , tail = ( 0, 0 )
                            , visited = Set.singleton ( 0, 0 )
                            }

                        expectedState =
                            { headRel = ( 0, 1 )
                            , tail = ( 1, 1 )
                            , visited = Set.fromList [ ( 0, 0 ), ( 1, 1 ) ]
                            }
                    in
                    Expect.equal
                        (applyCommandsToRopeState Up initialState)
                        expectedState
            , test "applyCommandsToRopeState moving head up has no affect on tail if it's within the boundary" <|
                \_ ->
                    let
                        initialState =
                            { headRel = ( 0, -1 )
                            , tail = ( 0, 0 )
                            , visited = Set.singleton ( 0, 0 )
                            }

                        expectedState =
                            { headRel = ( 0, 0 )
                            , tail = ( 0, 0 )
                            , visited = Set.fromList [ ( 0, 0 ) ]
                            }
                    in
                    Expect.equal
                        (applyCommandsToRopeState Up initialState)
                        expectedState

            --
            , test "applyCommandsToRopeState moves rope correctly down" <|
                \_ ->
                    let
                        expectedState =
                            { headRel = ( 0, -1 )
                            , tail = ( 0, 0 )
                            , visited = Set.fromList [ ( 0, 0 ) ]
                            }
                    in
                    Expect.equal
                        (applyCommandsToRopeState Down ropeStateZero)
                        expectedState
            , test "applyCommandsToRopeState moves rope correctly down when head's rel pos moves beyond boundary" <|
                \_ ->
                    let
                        initialState =
                            { headRel = ( 0, -1 )
                            , tail = ( 0, 0 )
                            , visited = Set.singleton ( 0, 0 )
                            }

                        expectedState =
                            { headRel = ( 0, -1 )
                            , tail = ( 0, -1 )
                            , visited = Set.fromList [ ( 0, 0 ), ( 0, -1 ) ]
                            }
                    in
                    Expect.equal
                        (applyCommandsToRopeState Down initialState)
                        expectedState
            , test "applyCommandsToRopeState moves rope correctly down when head's rel pos moves beyond boundary and it is offset" <|
                \_ ->
                    let
                        initialState =
                            { headRel = ( 1, -1 )
                            , tail = ( 0, 0 )
                            , visited = Set.singleton ( 0, 0 )
                            }

                        expectedState =
                            { headRel = ( 0, -1 )
                            , tail = ( 1, -1 )
                            , visited = Set.fromList [ ( 0, 0 ), ( 1, -1 ) ]
                            }
                    in
                    Expect.equal
                        (applyCommandsToRopeState Down initialState)
                        expectedState
            , test "applyCommandsToRopeState moving head DOWN has no affect on tail if it's within the boundary" <|
                \_ ->
                    let
                        initialState =
                            { headRel = ( 0, 1 )
                            , tail = ( 0, 0 )
                            , visited = Set.singleton ( 0, 0 )
                            }

                        expectedState =
                            { headRel = ( 0, 0 )
                            , tail = ( 0, 0 )
                            , visited = Set.fromList [ ( 0, 0 ) ]
                            }
                    in
                    Expect.equal
                        (applyCommandsToRopeState Down initialState)
                        expectedState

            -- Left
            , test "applyCommandsToRopeState moves rope correctly left" <|
                \_ ->
                    let
                        expectedState =
                            { headRel = ( -1, 0 )
                            , tail = ( 0, 0 )
                            , visited = Set.fromList [ ( 0, 0 ) ]
                            }
                    in
                    Expect.equal
                        (applyCommandsToRopeState Left ropeStateZero)
                        expectedState
            , test "applyCommandsToRopeState moves rope correctly left when head's rel pos moves beyond boundary" <|
                \_ ->
                    let
                        initialState =
                            { headRel = ( -1, 0 )
                            , tail = ( 0, 0 )
                            , visited = Set.singleton ( 0, 0 )
                            }

                        expectedState =
                            { headRel = ( -1, 0 )
                            , tail = ( -1, 0 )
                            , visited = Set.fromList [ ( 0, 0 ), ( -1, 0 ) ]
                            }
                    in
                    Expect.equal
                        (applyCommandsToRopeState Left initialState)
                        expectedState

            -- , test "applyCommandsToRopeState moves rope correctly left when head's rel pos moves beyond boundary and it is offset" <|
            --     \_ ->
            --         let
            --             initialState =
            --                 { headRel = ( -1, 1 )
            --                 , tail = ( 0, 0 )
            --                 , visited = Set.singleton ( 0, 0 )
            --                 }
            --             expectedState =
            --                 { headRel = ( -1, 1 )
            --                 , tail = ( 1, -1 )
            --                 , visited = Set.fromList [ ( 0, 0 ), ( 1, -1 ) ]
            --                 }
            --         in
            --         Expect.equal
            --             (applyCommandsToRopeState Down initialState)
            --             expectedState
            -- , test "applyCommandsToRopeState moving head DOWN has no affect on tail if it's within the boundary" <|
            --     \_ ->
            --         let
            --             initialState =
            --                 { headRel = ( 0, 1 )
            --                 , tail = ( 0, 0 )
            --                 , visited = Set.singleton ( 0, 0 )
            --                 }
            --             expectedState =
            --                 { headRel = ( 0, 0 )
            --                 , tail = ( 0, 0 )
            --                 , visited = Set.fromList [ ( 0, 0 ) ]
            --                 }
            --         in
            --         Expect.equal
            --             (applyCommandsToRopeState Down initialState)
            --             expectedState
            ]
        ]
