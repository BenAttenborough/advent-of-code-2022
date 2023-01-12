module Advent7 exposing (..)

import Advent7Data
import AlternativeSolutions.DirectoryParser exposing (Msg)
import Html exposing (Html, p, text)
import Html.Attributes exposing (class)
import Parser exposing (..)
import Tree exposing (tree)
import Tree.Zipper as Zipper
import Utilities.Utilities exposing (linesDebugToHtml, linesToHtml)


type alias Directory =
    { label : String
    , files : List File
    }


type alias File =
    { label : String
    , size : Int
    }


demoTree : Tree.Tree Directory
demoTree =
    Tree.singleton (Directory "root" [])


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


main : Html msg
main =
    Advent7Data.testInput
        |> String.split "$ "
        |> List.map (Parser.run commandParser)
        |> linesDebugToHtml



-- |> List.map
--     (\line ->
--         p [ class "command-line" ] [ text (Debug.toString line) ]
--     )
-- |> (\list ->
--         Html.div []
--             list
--    )
-- Html.text "foo"
-- demoTree
--     |> Zipper.fromTree
--     |> Zipper.mapTree
--         (addFolder
--             (tree (Directory "home" []) [])
--         )
--     |> Zipper.mapTree
--         (addFolder
--             (tree (Directory "var" []) [])
--         )
--     |> Zipper.findNext
--         (\x ->
--             x.label == "home"
--         )
--     |> Maybe.withDefault (Zipper.fromTree demoTree)
--     |> Zipper.mapTree
--         (addFolder
--             (tree (Directory "foo" []) [])
--         )
--     |> Zipper.mapTree
--         (addFolder
--             (tree (Directory "bar" []) [])
--         )
--     |> Zipper.toTree
--     |> Tree.restructure directoryToHtml toListItems
--     |> (\root -> Html.ul [] [ root ])


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
    | LS (List String)
    | Home
    | UpDir


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


statement : Parser String
statement =
    getChompedString <|
        succeed ()
            |. chompIf (\c -> c /= '\n')
            |. chompWhile (\c -> c /= '\n')


statementsHelper : List String -> Parser (Step (List String) (List String))
statementsHelper strings =
    oneOf
        [ succeed (\s -> Loop (s :: strings))
            |= statement
            |. symbol "\n"
        , succeed ()
            |> map (\_ -> Done (List.reverse strings))
        ]


statements : Parser (List String)
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
