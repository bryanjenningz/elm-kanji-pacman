module Utils exposing (..)


px : Int -> String
px x =
    String.fromInt x ++ "px"


get : Int -> List a -> Maybe a
get i list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if i <= 0 then
                Just first

            else
                get (i - 1) rest
