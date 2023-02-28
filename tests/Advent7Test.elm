module Advent7Test exposing (..)

import Advent7 exposing (..)
import Expect
import Parser exposing (Parser)
import Test exposing (..)


testInput : String
testInput =
    """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"""


suite : Test
suite =
    describe "Advent Day 7"
        [ describe "Part 1" <|
            [ test "Example" <|
                \_ -> Expect.equal True True

            -- , test "fileSizeParser with '14848513'" <|
            --     \_ ->
            --         Expect.equal
            --             (Ok 14848513)
            --             (Parser.run fileSizeParser "14848513")
            -- , test "fileSizeParser with '14848514 b.txt'" <|
            --     \_ ->
            --         Expect.equal
            --             (Ok 14848514)
            --             (Parser.run fileSizeParser "14848514 b.txt")
            -- , test "commandParser with '$ cd'" <|
            --     \_ ->
            --         Expect.equal
            --             (Ok CD)
            --             (Parser.run commandParser "$ cd")
            -- , test "commandParser with '$ ls'" <|
            --     \_ ->
            --         Expect.equal
            --             (Ok LS)
            --             (Parser.run commandParser "$ ls")
            -- , test "day7PartOne with testInput" <|
            --     \_ ->
            --         Expect.equal
            --             (Ok 14848514)
            --             (Parser.run fileSizeParser "14848514 b.txt")
            ]
        ]
