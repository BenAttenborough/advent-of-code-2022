module Advent7 exposing (..)

import Advent7Data
import AlternativeSolutions.DirectoryParser exposing (Msg)
import Html exposing (Html, p, text)
import Html.Attributes exposing (class)
import Parser exposing (..)
import Tree exposing (Tree, tree)
import Tree.Zipper as Zipper
import Utilities.DirectoryTree as DirectoryTree
import Utilities.Utilities exposing (linesDebugToHtml, linesToHtml)


type alias Directory =
    { label : String
    , files : List File
    }


type alias File =
    { label : String
    , size : Int
    }


emptyDirectory : Zipper.Zipper DirectoryTree.Directory
emptyDirectory =
    DirectoryTree.singleton (Directory "root" [])


directoryToHtml : Directory -> Html msg
directoryToHtml dir =
    Html.div []
        [ Html.p []
            [ Html.text (dir.label ++ " (DIR)") ]
        , Html.div []
            [ dir.files
                |> List.map (\file -> Html.li [] [ Html.text (file.label ++ " (" ++ String.fromInt file.size ++ ")") ])
                |> Html.ul []
            ]
        ]



-- treeToHtml : Tree.Tree Directory -> Html Msg
-- treeToHtml tree =
--     tree


toListItems : Html msg -> List (Html msg) -> Html msg
toListItems label children =
    case children of
        [] ->
            Html.li [] [ label ]

        _ ->
            Html.li []
                [ label
                , Html.ul [] children
                ]


updateDirectory : Tree.Tree Directory -> Tree.Tree Directory
updateDirectory dir =
    dir


getCommands : String -> List Command
getCommands commands =
    commands
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
                                                        FileType size name ->
                                                            Just (DirectoryTree.File name size)

                                                        _ ->
                                                            Nothing
                                                )
                                            |> List.filterMap identity

                                    directories =
                                        items
                                            |> List.map
                                                (\item ->
                                                    case item of
                                                        Dir name ->
                                                            Just (Tree.tree (Directory name []))

                                                        _ ->
                                                            Nothing
                                                )
                                            |> List.filterMap identity
                                in
                                newList
                                    |> DirectoryTree.addFiles files
                                    |> (\dirs directoryTree ->
                                            List.foldl
                                                addFolder
                                                directoryTree
                                                dirs
                                       )
                                        directories

                            CD _ ->
                                newList

                            _ ->
                                newList
                    )
                    tree
                    commands
           )
        |> DirectoryTree.toHtml


changeDirectory : String -> Zipper.Zipper Directory -> Maybe (Zipper.Zipper Directory)
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
                            x.label == "home"
                        )
                        haystack

                else
                    Just haystack
           )


addFolder : Tree.Tree Directory -> Tree.Tree Directory -> Tree.Tree Directory
addFolder child parent =
    case Tree.children parent of
        [] ->
            Tree.replaceChildren
                [ child ]
                parent

        _ ->
            Tree.prependChild
                child
                parent


addChildToDirectory parent child =
    let
        data =
            Tree.label parent

        title =
            data.label

        files =
            data.files
    in
    tree (Directory title files) [ child ]


type TerminalEntry
    = Command
    | Other


type Command
    = CD String
    | LS (List ItemType)
    | Home
    | UpDir
    | NoOp


type ItemType
    = Dir String
    | FileType Int String


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
        [ succeed Dir
            |. keyword "dir"
            |. spaces
            |= stringParser
        , succeed FileType
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
                    [ succeed CD
                        |= dirWord
                    , succeed Home
                        |. keyword "/"
                    , succeed UpDir
                        |. keyword ".."
                    ]
            , succeed LS
                |. keyword "ls"
                |. spaces
                |= statements
            ]
