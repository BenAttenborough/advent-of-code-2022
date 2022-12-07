module Advent7 exposing (..)

import Parser exposing (..)


type Command
    = CD
    | LS


type File
    = File Int String


type Directory
    = Directory String


type FileType
    = DirectoryObject Directory
    | FileObject File


type alias DirectoryContent =
    { files : List File
    , directories : List Directory
    }



-- fileParser : Parser DirectoryContent
-- fileParser =
--     succeed identity
--         |


fileSizeParser : Parser Int
fileSizeParser =
    succeed identity
        |= int


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



-- commandParser : Parser Command
-- commandParser =
--     oneOf
--         [ map (\_ -> CD) (keyword "cd")
--         , map (\_ -> LS) (keyword "ls")
--         ]
-- commandParser : Parser Command
-- commandParser =
--     oneOf
--         [ map (\_ -> CD) (keyword "CD")
--         , map (\_ -> LS) (keyword "LS")
--         ]
-- commandParser : Parser Command
-- commandParser =
--     succeed Command
--         |= oneOf
--             [ succeed CD
--                 |. keyword "CD"
--             ]
-- day7PartOne : String -> Int


day7PartOne input =
    input
        |> String.lines
