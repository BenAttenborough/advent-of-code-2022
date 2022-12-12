module ParsersTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser exposing (..)
import Parsers.Parsers exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Parsers"
        [ test "Example" <|
            \_ -> Expect.equal True True
        , test "listParserStep" <|
            \_ ->
                Expect.equal
                    (Ok [ "turtles", "lizards" ])
                    (Parser.run listParser
                        "- turtles\n- lizards"
                    )
        ]
