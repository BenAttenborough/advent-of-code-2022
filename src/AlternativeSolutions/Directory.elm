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
        |> Directory.changeDirectory "var"
        |> Directory.addFolder (Directory.Directory "a" [])
        -- We should force addFolder to refuse to ad directory with same name as one
        -- that already exists (at that level)
        |> Directory.addFolder (Directory.Directory "a" [])
        |> toHtml
