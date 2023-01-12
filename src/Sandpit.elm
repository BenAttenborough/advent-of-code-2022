module Sandpit exposing (..)

import Html exposing (Html)
import Parser exposing (..)


type Stmt
    = Stmt String


commands : String
commands =
    "qwerty; another; nail; horse; rabbit;"


main : Html msg
main =
    -- Html.text "Hello World"
    commands
        |> Parser.run statements
        |> (\s -> Html.text (Debug.toString s))


statements : Parser (List String)
statements =
    loop [] statementsHelp


statementsHelp : List String -> Parser (Step (List String) (List String))
statementsHelp revStmts =
    oneOf
        [ succeed (\stmt -> Loop (stmt :: revStmts))
            |= statement
            |. spaces
            |. symbol ";"
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse revStmts))
        ]


statement : Parser String
statement =
    getChompedString <|
        succeed ()
            |. chompIf (\c -> Char.isAlphaNum c)
            |. chompWhile (\c -> Char.isAlphaNum c)
