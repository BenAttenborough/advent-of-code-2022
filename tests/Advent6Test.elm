module Advent6Test exposing (..)

import Advent6 exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Advent Day 6"
        [ describe "allUniqueCharacters"
            [ test "abcd" <|
                \_ -> Expect.equal True (allUniqueCharacters "abcd")
            , test "abdb" <|
                \_ -> Expect.equal False (allUniqueCharacters "abcb")
            ]
        , describe "applyToWindow"
            [ test "Four unique characters" <|
                \_ ->
                    Expect.equal
                        (Ok 3)
                        (applyToWindow
                            4
                            (\list -> allUniqueCharacters (String.fromList list))
                            0
                            [ 'a', 'b', 'b', 'b', 'c', 'd', 'e' ]
                        )
            , test "No four characters in a row are unique" <|
                \_ ->
                    Expect.equal
                        (Err "Pattern not found")
                        (applyToWindow
                            4
                            (\list -> allUniqueCharacters (String.fromList list))
                            0
                            [ 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a' ]
                        )
            , test "mjqjpqmgbljsphdztnvjfqwrcgsmlb = 3" <|
                \_ ->
                    Expect.equal
                        (Ok 3)
                        (applyToWindow
                            4
                            (\list -> allUniqueCharacters (String.fromList list))
                            0
                            (String.toList "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
                        )
            , test "bvwbjplbgvbhsrlpgdmjqwftvncz = 1" <|
                \_ ->
                    Expect.equal
                        (Ok 1)
                        (applyToWindow
                            4
                            (\list -> allUniqueCharacters (String.fromList list))
                            0
                            (String.toList "bvwbjplbgvbhsrlpgdmjqwftvncz")
                        )
            ]
        , describe "day6Part1"
            [ test "mjqjpqmgbljsphdztnvjfqwrcgsmlb = 7" <|
                \_ ->
                    Expect.equal
                        (Ok 7)
                        (day6Part1 4 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
            , test "bvwbjplbgvbhsrlpgdmjqwftvncz = 5" <|
                \_ ->
                    Expect.equal
                        (Ok 5)
                        (day6Part1 4 "bvwbjplbgvbhsrlpgdmjqwftvncz")
            , test "nppdvjthqldpwncqszvftbrmjlhg = 6" <|
                \_ ->
                    Expect.equal
                        (Ok 6)
                        (day6Part1 4 "nppdvjthqldpwncqszvftbrmjlhg")
            , test "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg = 10" <|
                \_ ->
                    Expect.equal
                        (Ok 10)
                        (day6Part1 4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
            , test "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw = 11" <|
                \_ ->
                    Expect.equal
                        (Ok 11)
                        (day6Part1 4 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
            ]
        , describe "day6Part2"
            [ test "mjqjpqmgbljsphdztnvjfqwrcgsmlb = 19" <|
                \_ ->
                    Expect.equal
                        (Ok 19)
                        (day6Part1 14 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
            , test "bvwbjplbgvbhsrlpgdmjqwftvncz = 23" <|
                \_ ->
                    Expect.equal
                        (Ok 23)
                        (day6Part1 14 "bvwbjplbgvbhsrlpgdmjqwftvncz")
            , test "nppdvjthqldpwncqszvftbrmjlhg = 23" <|
                \_ ->
                    Expect.equal
                        (Ok 23)
                        (day6Part1 14 "nppdvjthqldpwncqszvftbrmjlhg")
            , test "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg = 29" <|
                \_ ->
                    Expect.equal
                        (Ok 29)
                        (day6Part1 14 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
            , test "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw = 26" <|
                \_ ->
                    Expect.equal
                        (Ok 26)
                        (day6Part1 14 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
            ]
        ]
