module Advent7 exposing (..)

-- import Utilities.DirectoryTree exposing (..)

import Advent7Data
import AlternativeSolutions.DirectoryParser exposing (Msg)
import Html exposing (Html, p, text)
import Html.Attributes exposing (class, style)
import Json.Decode exposing (maybe)
import Parser exposing (..)
import Tree exposing (Tree, tree)
import Tree.Zipper as Zipper
import Utilities.Utilities exposing (linesDebugToHtml, linesToHtml)


type alias Directory =
    { name : String
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
                                        Zipper.label newList
                                in
                                newList
                                    |> Zipper.replaceLabel
                                        { data | size = size }
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
        |> toHtml


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


toHtml : Zipper.Zipper Directory -> Html msg
toHtml dir =
    let
        directoryStructure =
            dir
                |> Zipper.tree
                |> Tree.restructure directoryToHtml toListItems
                |> (\root -> Html.ul [] [ root ])

        commands =
            getCommands Advent7Data.testInput
    in
    Html.div
        [ style "display" "grid"
        , style "grid-template-columns" "1fr 1fr 1fr"
        , style "height" "100vh"
        ]
        [ Html.div
            [ style "border" "solid 2px green"
            , style "padding" "0.5rem"
            ]
            [ Html.text "Commands"
            , linesDebugToHtml commands
            ]
        , Html.div
            [ style "border" "solid 2px green"
            , style "border-left" "none"
            , style "border-right" "none"
            , style "padding" "0.5rem"
            ]
            [ Html.text "Directory tree"
            , directoryStructure
            ]
        , Html.div
            [ style "border" "solid 2px green"
            , style "padding" "0.5rem"
            ]
            [ Html.text "Debug" ]
        ]


directoryToHtml : Directory -> Html msg
directoryToHtml dir =
    Html.div []
        [ Html.p []
            [ Html.text (dir.name ++ " (DIR)") ]
        , Html.div []
            [ Html.text (String.fromInt dir.size) ]
        ]


childLabelConflictsWithExisting : Tree Directory -> List (Tree Directory) -> Bool
childLabelConflictsWithExisting child children =
    let
        childData =
            Tree.label child

        childrenLabels =
            List.map (\x -> Tree.label x) children
    in
    List.any (\item -> item.name == childData.name) childrenLabels


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
                        data.name == needle
                    )
    in
    haystack
        |> Zipper.children
        |> (\children ->
                if isNeedleInHaystack children then
                    Zipper.findNext
                        (\x ->
                            x.name == needle
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
            x.name == needle
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
