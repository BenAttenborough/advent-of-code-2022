module AlternativeSolutions.Directory exposing (..)

import Html exposing (Html)
import Tree.Zipper as Zipper
import Utilities.DirectoryTree as Directory exposing (..)


demoTree : Zipper.Zipper Directory
demoTree =
    singleton (Directory "root" [])


main : Html msg
main =
    demoTree
        |> addFolder (Directory "home" [])
        |> addFolder (Directory "var" [])
        |> changeDirectory "var"
        |> addFolder (Directory "a" [])
        |> addFile (File "Main.elm" 100)
        |> changeDirectory "a"
        |> addFiles
            [ File "Main.elm" 200
            , File "README.md" 100
            ]
        |> toHtml
