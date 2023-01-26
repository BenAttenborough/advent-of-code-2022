module Advent7 exposing (..)

-- import Utilities.DirectoryTree exposing (..)

import Advent7Data
import AlternativeSolutions.DirectoryParser exposing (Msg)
import Html exposing (Html, p, text)
import Html.Attributes exposing (class)
import Json.Decode exposing (maybe)
import Parser exposing (..)
import Tree exposing (Tree, tree)
import Tree.Zipper as Zipper
import Utilities.Utilities exposing (linesDebugToHtml, linesToHtml)


type alias Directory =
    { label : String
    , size : Int
    }


emptyDirectory : Zipper.Zipper Directory
emptyDirectory =
    Tree.singleton (Directory "root" 0)
        |> Zipper.fromTree


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
                                    size =
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
                                            |> List.sum

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

                                    data =
                                        Zipper.label tree
                                in
                                -- newList
                                Zipper.replaceLabel
                                    { data | size = size }
                                    newList
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
        |> Zipper.root
        |> Debug.toString
        |> Html.text



-- |> toHtml


childLabelConflictsWithExisting : Tree Directory -> List (Tree Directory) -> Bool
childLabelConflictsWithExisting child children =
    let
        childData =
            Tree.label child

        childrenLabels =
            List.map (\x -> Tree.label x) children
    in
    List.any (\item -> item.label == childData.label) childrenLabels


addFolderInternal : Tree.Tree Directory -> Tree.Tree Directory -> Tree.Tree Directory
addFolderInternal child parent =
    case Tree.children parent of
        [] ->
            Tree.replaceChildren
                [ child ]
                parent

        children ->
            if childLabelConflictsWithExisting child children then
                parent

            else
                Tree.prependChild
                    child
                    parent


addFolder : Directory -> Zipper.Zipper Directory -> Zipper.Zipper Directory
addFolder folder parent =
    parent
        |> Zipper.mapTree
            (addFolderInternal
                (tree folder [])
            )


changeDirectory : String -> Zipper.Zipper Directory -> Zipper.Zipper Directory
changeDirectory needle haystack =
    let
        isNeedleInHaystack list =
            list
                |> List.any
                    (\child ->
                        let
                            data =
                                Tree.label child
                        in
                        data.label == needle
                    )
    in
    haystack
        |> Zipper.children
        |> (\children ->
                if isNeedleInHaystack children then
                    Zipper.findNext
                        (\x ->
                            x.label == needle
                        )
                        haystack
                        |> Maybe.withDefault haystack

                else
                    haystack
           )


goForward x =
    x
        |> Zipper.forward
        |> Maybe.withDefault x


goChangeDir needle haystack =
    Zipper.findNext
        (\x ->
            x.label == needle
        )
        haystack
        |> Maybe.withDefault haystack


type Command
    = CD String
    | LS (List ItemType)
    | Home
    | UpDir
    | NoOp


type ItemType
    = Dir Directory
    | FileType Int


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
        [ succeed (\x -> Dir (Directory x 0))
            |. keyword "dir"
            |. spaces
            |= stringParser
        , succeed FileType
            |= int
            |. spaces
            |. fileNameParser
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
