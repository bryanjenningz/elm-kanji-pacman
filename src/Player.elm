module Player exposing (..)

import Direction exposing (Direction(..))
import Html exposing (Html)
import Html.Attributes exposing (style)
import Screen exposing (spotSize)
import Utils exposing (px)


type alias Player =
    { x : Int
    , y : Int
    , direction : Direction
    }


initPlayer : Player
initPlayer =
    { x = 7 * spotSize
    , y = 11 * spotSize
    , direction = Left
    }


viewPlayer : Player -> Html msg
viewPlayer player =
    Html.div
        [ style "position" "absolute"
        , style "background-color" playerColor
        , style "width" (px spotSize)
        , style "height" (px spotSize)
        , style "left" (px player.x)
        , style "top" (px player.y)
        ]
        []


playerColor : String
playerColor =
    "#1678aa"


playerSpeed : Int
playerSpeed =
    2
