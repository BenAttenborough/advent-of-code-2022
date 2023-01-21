module Advent7 exposing (..)

import Advent7Data
import AlternativeSolutions.DirectoryParser exposing (Msg)
import Html exposing (Html, p, text)
import Html.Attributes exposing (class)
import Parser exposing (..)
import Tree exposing (Tree, tree)
import Tree.Zipper as Zipper
import Utilities.DirectoryTree exposing (..)
import Utilities.Utilities exposing (linesDebugToHtml, linesToHtml)


emptyDirectory : Zipper.Zipper Directory
emptyDirectory =
    singleton (Directory "root" [])


getCommands : String -> List Command
getCommands commands =
    commands
        |> (\x -> String.append x "\n")
        |> String.split "$ "
        |> List.map (Parser.run commandParser)
        |> List.map (Result.withDefault NoOp)


main : Html msg
main =
    let
        commands =
            getCommands Advent7Data.testInput
    in
    -- commands
    --     |> linesDebugToHtml
    emptyDirectory
        |> (\tree ->
                List.foldl
                    (\command newList ->
                        case command of
                            Home ->
                                Zipper.root newList

                            LS items ->
                                let
                                    files =
                                        items
                                            |> List.map
                                                (\item ->
                                                    case item of
                                                        FileType file ->
                                                            Just file

                                                        _ ->
                                                            Nothing
                                                )
                                            |> List.filterMap identity

                                    directories =
                                        items
                                            |> List.map
                                                (\item ->
                                                    case item of
                                                        Dir directory ->
                                                            Just directory

                                                        _ ->
                                                            Nothing
                                                )
                                            |> List.filterMap identity
                                in
                                newList
                                    |> addFiles files
                                    |> (\dirs directoryTree ->
                                            List.foldl
                                                addFolder
                                                directoryTree
                                                dirs
                                       )
                                        directories

                            CD name ->
                                newList
                                    |> changeDirectory name

                            UpDir ->
                                newList
                                    |> Zipper.backward
                                    |> Maybe.withDefault newList

                            _ ->
                                newList
                    )
                    tree
                    commands
           )
        |> toHtml


type Command
    = CD String
    | LS (List ItemType)
    | Home
    | UpDir
    | NoOp


type ItemType
    = Dir Directory
    | FileType File


dirWord : Parser String
dirWord =
    getChompedString <|
        succeed ()
            |. chompIf (\c -> Char.isAlphaNum c || c == '.')
            |. chompWhile (\c -> Char.isAlphaNum c || c == '.')


word : Parser String
word =
    getChompedString <|
        succeed ()
            |. chompIf (\c -> Char.isAlphaNum c)
            |. chompWhile (\c -> Char.isAlphaNum c || c == '.')


stringParser : Parser String
stringParser =
    getChompedString <|
        succeed ()
            |. chompIf (\c -> c /= '\n')
            |. chompWhile (\c -> c /= '\n')


fileNameParser : Parser String
fileNameParser =
    getChompedString <|
        succeed ()
            |. chompIf (\c -> Char.isAlphaNum c)
            |. chompWhile (\c -> Char.isAlphaNum c || c == '.')


statement : Parser ItemType
statement =
    oneOf
        [ succeed (\x -> Dir (Directory x []))
            |. keyword "dir"
            |. spaces
            |= stringParser
        , succeed (\a b -> FileType (File b a))
            |= int
            |. spaces
            |= fileNameParser
        ]


statementsHelper : List ItemType -> Parser (Step (List ItemType) (List ItemType))
statementsHelper strings =
    oneOf
        [ succeed (\s -> Loop (s :: strings))
            |= statement
            |. symbol "\n"
        , succeed ()
            |> map (\_ -> Done (List.reverse strings))
        ]


statements : Parser (List ItemType)
statements =
    loop [] statementsHelper


commandParser : Parser Command
commandParser =
    succeed identity
        |= oneOf
            [ succeed identity
                |. keyword "cd"
                |. spaces
                |= oneOf
                    [ succeed UpDir
                        |. keyword ".."
                    , succeed Home
                        |. keyword "/"
                    , succeed CD
                        |= dirWord
                    ]
            , succeed LS
                |. keyword "ls"
                |. spaces
                |= statements
            ]
