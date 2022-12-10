module Advent8 exposing (..)


charToInt : Char -> Maybe Int
charToInt char =
    if Char.isDigit char then
        Just (Char.toCode char - 0x30)

    else
        Nothing


compareVerticalRows input =
    input



-- |> List.head
-- case rows of
--     [] ->
--         []
--     head :: rest ->
--         []


day8Part1 input =
    input
        |> String.lines
        |> List.map String.toList
        |> List.map (List.filterMap charToInt)
        -- Turn each tree into tuple. Second value used to indicate if tree has been counted
        -- I've just realised this probably isn't useful
        |> List.map (List.map (\tree -> ( tree, False )))


numberOfVisibleTrees : List ( Int, Bool ) -> Int -> Int -> List ( Int, Bool ) -> ( Int, List ( Int, Bool ) )
numberOfVisibleTrees trees currentHighest count processedTrees =
    case trees of
        [] ->
            ( count, List.reverse processedTrees )

        head :: rest ->
            if Tuple.second head then
                numberOfVisibleTrees rest currentHighest count (head :: processedTrees)

            else if Tuple.first head > currentHighest then
                numberOfVisibleTrees rest (Tuple.first head) (count + 1) (( Tuple.first head, True ) :: processedTrees)

            else
                numberOfVisibleTrees rest currentHighest count (head :: processedTrees)



-- |> compareVerticalRows
-- |> List.filterMap List.head
