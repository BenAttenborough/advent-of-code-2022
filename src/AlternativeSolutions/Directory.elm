module AlternativeSolutions.Directory exposing (..)

import Html exposing (Html)
import Tree
import Tree.Zipper as Zipper
import Utilities.DirectoryTree as Directory exposing (..)


demoTree : Zipper.Zipper Directory
demoTree =
    Directory.singleton (Directory.Directory "root" [])


main : Html msg
main =
    demoTree
        |> Directory.addFolder (Directory.Directory "home" [])
        |> Directory.addFolder (Directory.Directory "var" [])
        |> toHtml
