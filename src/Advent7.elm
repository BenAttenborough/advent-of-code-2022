module Advent7 exposing (..)

import Parser exposing (..)


type Command
    = CD
    | LS


type Directory
    = Empty String Int
    | Node String Int (List Directory)



-- type File
--     = File Int String
-- type Directory
--     = Directory String
-- type FileType
--     = DirectoryObject Directory
--     | FileObject File
-- type alias DirectoryContent =
--     { files : List File
--     , directories : List Directory
--     }
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



-- lineParser : Parser a
-- lineParser =
--     succeed identity
--         |= oneOf
--             [ commandParser
--             , fileSizeParser
--             ]
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


directoryA : Directory
directoryA =
    Node "/"
        56
        [ Node "a"
            2
            [ Empty "b1" 5
            , Empty "b2" 15
            , Node "b3"
                23
                [ Empty "c1" 3 ]
            ]
        ]


type alias State =
    { currentDir : String
    , size : Int
    , children : List Directory
    }



-- createDirectory


testFunction : List String -> String
testFunction entries =
    let
        finish entry next =
            next (entry :: entries)
    in
    "abc"
