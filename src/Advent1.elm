module Advent1 exposing (..)

import Advent1Data exposing (..)
import List exposing (foldl)
import Parser exposing (..)


blockTwo : Parser (List Int)
blockTwo =
    Parser.sequence
        { start = "[,"
        , separator = ","
        , end = "]"
        , spaces = spaces
        , item = int
        , trailing = Optional
        }


looper : Parser (List (List Int))
looper =
    loop [] loopHelper


loopHelper revStmts =
    oneOf
        [ succeed (\stmt -> Loop (stmt :: revStmts))
            |= blockTwo
            |. spaces
            |. symbol ";"
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse revStmts))
        ]


getBiggestRations data =
    String.lines data
        |> List.map
            (\string ->
                if string == "" then
                    "];["

                else
                    string
            )
        |> List.intersperse ","
        |> List.foldr String.append ""
        |> (++) "[,"
        |> (\string -> string ++ ",];")
        |> Parser.run looper
        |> Result.map (List.map (foldl (+) 0))
        |> Result.map
            (foldl
                (\a b ->
                    if b > a then
                        b

                    else
                        a
                )
                0
            )


getTopThreeRations data =
    String.lines data
        |> List.map
            (\string ->
                if string == "" then
                    "];["

                else
                    string
            )
        |> List.intersperse ","
        |> List.foldr String.append ""
        |> (++) "[,"
        |> (\string -> string ++ ",];")
        |> Parser.run looper
        |> Result.map (List.map (foldl (+) 0))
        |> Result.map List.sort
        |> Result.map List.reverse
        |> Result.map (List.take 3)
        |> Result.map List.sum
