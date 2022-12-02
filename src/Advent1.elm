module Advent1 exposing (..)

import Advent1Data exposing (test)
import Parser exposing ((|.), (|=), Parser, Step, end, int, loop, map, oneOf, spaces, succeed, symbol)


final : String
final =
    test


valueParser : Parser Int
valueParser =
    succeed identity
        |= int
        |. end


result : Result (List Parser.DeadEnd) Int
result =
    Parser.run valueParser "1234 567"


valuesParser : Parser (List Int)
valuesParser =
    loop [] valuesParserHelp


valuesParserHelp : List Int -> Parser (Step (List Int) (List Int))
valuesParserHelp list =
    oneOf
        [ succeed (\int -> Parser.Loop (int :: list))
            |= int
            |. spaces
        , succeed ()
            |> map (\_ -> Parser.Done (List.reverse list))
        ]


resultTwo : Result (List Parser.DeadEnd) (List Int)
resultTwo =
    Parser.run valuesParser test
