module Utilities.TreeViewer exposing (..)

import Html exposing (Html)
import Parser exposing (..)
import Tree exposing (tree)


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
