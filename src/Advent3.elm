module Advent3 exposing (..)

import Html
import Html.Attributes exposing (list)
import Set


main : Html.Html msg
main =
    Html.text "Hello!"


dayThreeTestInput : String
dayThreeTestInput =
    """vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"""


dayThreeTestInputPartTwo : String
dayThreeTestInputPartTwo =
    """vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg"""


type alias Container =
    { a : List Char
    , b : List Char
    }


charToInt : Char -> Int
charToInt char =
    if Char.isUpper char then
        Char.toCode char - 64 + 26

    else
        Char.toCode char - 96


dayThreePartOne : String -> Int
dayThreePartOne input =
    input
        |> String.lines
        |> List.map splitStringInTwo
        |> List.map (\list -> Set.intersect (Tuple.first list) (Tuple.second list))
        |> List.map Set.toList
        |> List.concat
        |> List.map charToInt
        |> List.sum



-- splitStringInTwo : String -> Set.Set String


splitStringInTwo : String -> ( Set.Set Char, Set.Set Char )
splitStringInTwo string =
    let
        midPoint =
            String.length string // 2

        endPoint =
            String.length string

        containerA =
            String.slice 0 midPoint string

        containerB =
            String.slice midPoint endPoint string
    in
    ( Set.fromList (String.toList containerA), Set.fromList (String.toList containerB) )


splitListBy : Int -> List (List a) -> List a -> List (List a)
splitListBy n newList list =
    case list of
        [] ->
            newList

        [ _ ] ->
            newList

        _ :: _ :: _ ->
            let
                a =
                    List.take n list

                b =
                    List.drop n list
            in
            splitListBy n (List.append [ a ] newList) b


setIntersection3 : List (Set.Set comparable) -> List comparable
setIntersection3 list =
    case list of
        [ a, b, c ] ->
            Set.intersect a b
                |> Set.intersect c
                |> Set.toList

        _ ->
            []


dayThreePartTwo : String -> Int
dayThreePartTwo input =
    input
        |> String.lines
        |> splitListBy 3 []
        |> List.map (List.map String.toList)
        |> List.map (List.map Set.fromList)
        |> List.map setIntersection3
        |> List.concat
        |> List.map charToInt
        |> List.sum
