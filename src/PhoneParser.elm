module PhoneParser exposing (..)

import Html
import Parser exposing (..)


main =
    Html.text "Hello!"


whitespace : Parser ()
whitespace =
    chompWhile (\c -> c == ' ')


countryCode : Parser Int
countryCode =
    succeed identity
        |. spaces
        |. symbol "+"
        |= int
        |. spaces


countryCodeOptional : Parser (Maybe Int)
countryCodeOptional =
    oneOf
        [ succeed Just
            |. whitespace
            |. symbol "+"
            |= int
            |. whitespace
        , succeed Nothing
        ]


runCountryCode : String -> Result (List Parser.DeadEnd) Int
runCountryCode code =
    run countryCode code


runCountryCodeOptional : String -> Result (List Parser.DeadEnd) (Maybe Int)
runCountryCodeOptional code =
    run countryCodeOptional code



-- > runCountryCodeOptional "+34445"
-- Ok (Just 34445)
--     : Result (List Parser.DeadEnd) (Maybe Int)
-- > runCountryCodeOptional "34445"
-- Ok Nothing : Result (List Parser.DeadEnd) (Maybe Int)


areaCode : Parser (Maybe Int)
areaCode =
    oneOf
        [ succeed String.toInt
            |. symbol "("
            |. whitespace
            |= (getChompedString <| chompWhile Char.isDigit)
            |. whitespace
            |. symbol ")"
            |. whitespace
        , succeed Nothing
        ]


runAreaCode : String -> Result (List Parser.DeadEnd) (Maybe Int)
runAreaCode code =
    run areaCode code
