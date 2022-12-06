module Advent6 exposing (..)

import Advent6Data exposing (..)
import Set


allUniqueCharacters : String -> Bool
allUniqueCharacters string =
    let
        numberOfCharacters =
            String.length string

        numberOfUniqueCharacters =
            string
                |> String.toList
                |> Set.fromList
                |> Set.size
    in
    numberOfCharacters == numberOfUniqueCharacters


applyToWindow : Int -> (List a -> Bool) -> Int -> List a -> Result String Int
applyToWindow windowSize test count list =
    let
        listLength =
            List.length list

        window =
            List.take windowSize list
    in
    if listLength < windowSize then
        Err "Pattern not found"

    else if test window then
        Ok count

    else
        applyToWindow windowSize test (count + 1) (List.drop 1 list)


day6Part1 : Int -> String -> Result String Int
day6Part1 windowSize input =
    applyToWindow
        windowSize
        (\list -> allUniqueCharacters (String.fromList list))
        0
        (String.toList input)
        |> Result.map ((+) windowSize)
