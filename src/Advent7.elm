module Advent7 exposing (..)

import Advent7Data
import Html exposing (Html)
import Parser exposing (..)
import Tree exposing (tree)
import Tree.Zipper as Zipper


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
    tree (Directory "root" [ File "a" 123, File "b" 54, File "c" 5432 ])
        [ tree (Directory "home" [])
            [ tree (Directory "user1" []) []
            , tree (Directory "user2" []) []
            ]
        , tree (Directory "etc" [ File "d" 5345, File "e" 24, File "f" 428 ]) []
        , tree (Directory "var" [])
            [ tree (Directory "log" []) []
            ]
        ]


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
    demoTree
        |> Tree.restructure directoryToHtml toListItems
        |> (\root -> Html.ul [] [ root ])



-- |> Zipper.fromTree
-- |> treeRecurrser [ Tree.label demoTree ] "-"
-- |> List.map
--     (\item ->
--         Html.div []
--             [ Html.text item ]
--     )
-- |> Html.div []
-- |> Debug.toString
-- |> Html.text


treeRecurrser agg preprend tree =
    case Zipper.forward tree of
        Just a ->
            treeRecurrser ((preprend ++ Zipper.label a) :: agg) (preprend ++ "-") a

        Nothing ->
            List.reverse agg


type TerminalEntry
    = Command
    | Other


type Command
    = CD
    | LS


commandParser : Parser Command
commandParser =
    succeed identity
        |. symbol "$"
        |. spaces
        |= oneOf
            [ succeed CD
                |. keyword "cd"
            , succeed LS
                |. keyword "ls"
            ]


fileSizeParser : Parser Int
fileSizeParser =
    succeed identity
        |= int



-- lineParser : Parser a
-- lineParser =
--     succeed TerminalEntry
--         |= oneOf
--             [ commandParser
--             , fileSizeParser
--             ]
-- type Directory
--     = Empty String Int
--     | Node String Int (List Directory)
-- -- type File
-- --     = File Int String
-- -- type Directory
-- --     = Directory String
-- -- type FileType
-- --     = DirectoryObject Directory
-- --     | FileObject File
-- -- type alias DirectoryContent =
-- --     { files : List File
-- --     , directories : List Directory
-- --     }
-- -- fileParser : Parser DirectoryContent
-- -- fileParser =
-- --     succeed identity
-- --         |
-- -- commandParser : Parser Command
-- -- commandParser =
-- --     oneOf
-- --         [ map (\_ -> CD) (keyword "cd")
-- --         , map (\_ -> LS) (keyword "ls")
-- --         ]
-- -- commandParser : Parser Command
-- -- commandParser =
-- --     oneOf
-- --         [ map (\_ -> CD) (keyword "CD")
-- --         , map (\_ -> LS) (keyword "LS")
-- --         ]
-- -- commandParser : Parser Command
-- -- commandParser =
-- --     succeed Command
-- --         |= oneOf
-- --             [ succeed CD
-- --                 |. keyword "CD"
-- --             ]
-- -- day7PartOne : String -> Int
-- day7PartOne input =
--     input
--         |> String.lines
-- directoryA : Directory
-- directoryA =
--     Node "/"
--         56
--         [ Node "a"
--             2
--             [ Empty "b1" 5
--             , Empty "b2" 15
--             , Node "b3"
--                 23
--                 [ Empty "c1" 3 ]
--             ]
--         ]
-- type alias State =
--     { currentDir : String
--     , size : Int
--     , children : List Directory
--     }
-- -- createDirectory
-- testFunction : List String -> String
-- testFunction entries =
--     let
--         finish entry next =
--             next (entry :: entries)
--     in
--     "abc"
