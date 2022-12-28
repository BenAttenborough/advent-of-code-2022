module Utilities.DirectoryTree exposing (Directory, DirectoryTree, addFolder, changeDirectory, singleton, toHtml)

import Html exposing (Html)
import Parser exposing (..)
import Tree exposing (Tree, tree)
import Tree.Zipper as Zipper


type alias Directory =
    { label : String
    , files : List File
    }


type alias File =
    { label : String
    , size : Int
    }


type DirectoryTree a
    = DirectoryTree Directory (List (DirectoryTree Directory))


singleton : Directory -> Zipper.Zipper Directory
singleton directory =
    Tree.singleton directory
        |> Zipper.fromTree


toHtml : Zipper.Zipper Directory -> Html msg
toHtml dir =
    dir
        |> Zipper.toTree
        |> Tree.restructure directoryToHtml toListItems
        |> (\root -> Html.ul [] [ root ])


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


addFolder : Directory -> Zipper.Zipper Directory -> Zipper.Zipper Directory
addFolder folder parent =
    parent
        |> Zipper.mapTree
            (addFolderInternal
                (tree folder [])
            )


addFolderInternal : Tree.Tree Directory -> Tree.Tree Directory -> Tree.Tree Directory
addFolderInternal child parent =
    case Tree.children parent of
        [] ->
            Tree.replaceChildren
                [ child ]
                parent

        _ ->
            -- This really needs to check no siblings have the same "label" before adding child
            Tree.prependChild
                child
                parent


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
                            x.label == "home"
                        )
                        haystack
                        |> Maybe.withDefault haystack

                else
                    haystack
           )
