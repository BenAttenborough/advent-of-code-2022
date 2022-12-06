module Advent5 exposing (..)

import Array exposing (Array)
import Parser exposing (..)


updateNestedList : Int -> (List a -> List a) -> List (List a) -> List (List a)
updateNestedList index fnc nestedList =
    nestedList
        |> Array.fromList
        |> Array.indexedMap
            (\i element ->
                if i == index then
                    fnc element

                else
                    element
            )
        |> Array.toList


sliceList : Int -> Int -> List a -> List a
sliceList start end list =
    Array.slice start end (Array.fromList list) |> Array.toList


initialTestState : List (List Char)
initialTestState =
    [ [ 'N', 'Z' ]
    , [ 'D', 'C', 'M' ]
    , [ 'P' ]
    ]


testInstructions : String
testInstructions =
    """move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"""


type alias Instruction =
    { move : Int
    , from : Int
    , to : Int
    }


instructionParser : Parser Instruction
instructionParser =
    succeed Instruction
        |. keyword "move"
        |. spaces
        |= int
        |. spaces
        |. keyword "from"
        |. spaces
        |= int
        |. spaces
        |. keyword "to"
        |. spaces
        |= int


getListItemByIndex : Int -> List a -> Maybe a
getListItemByIndex index list =
    list
        |> Array.fromList
        |> Array.get index


moveCrates crates instructions =
    case instructions of
        [] ->
            crates

        instruction :: rest ->
            let
                cratesToMove =
                    getListItemByIndex (instruction.from - 1) crates
                        -- |> Debug.log "getListItemByIndex"
                        |> Maybe.withDefault []
                        |> sliceList 0 instruction.move

                -- |> Debug.log "crates to move"
                newList =
                    crates
                        -- |> Debug.log "Initial configuration"
                        |> updateNestedList (instruction.from - 1) (sliceList instruction.move 50000)
                        -- |> Debug.log "After crate removal"
                        |> updateNestedList (instruction.to - 1) (List.append (List.reverse cratesToMove))
                        |> Debug.log "After crate append"
            in
            moveCrates newList rest


dayFive instructions initialCrates =
    instructions
        |> String.lines
        |> List.map (Parser.run instructionParser)
        |> List.map Result.toMaybe
        |> List.filterMap identity
        |> moveCrates initialCrates
        |> List.filterMap List.head


moveCrates2 crates instructions =
    case instructions of
        [] ->
            crates

        instruction :: rest ->
            let
                cratesToMove =
                    getListItemByIndex (instruction.from - 1) crates
                        -- |> Debug.log "getListItemByIndex"
                        |> Maybe.withDefault []
                        |> sliceList 0 instruction.move

                -- |> Debug.log "crates to move"
                newList =
                    crates
                        -- |> Debug.log "Initial configuration"
                        |> updateNestedList (instruction.from - 1) (sliceList instruction.move 50000)
                        -- |> Debug.log "After crate removal"
                        |> updateNestedList (instruction.to - 1) (List.append cratesToMove)
                        |> Debug.log "After crate append>"
            in
            moveCrates2 newList rest


dayFive2 instructions initialCrates =
    instructions
        |> String.lines
        |> List.map (Parser.run instructionParser)
        |> List.map Result.toMaybe
        |> List.filterMap identity
        |> moveCrates2 initialCrates
        |> List.filterMap List.head
