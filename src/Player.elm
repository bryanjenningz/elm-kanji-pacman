module Player exposing (Player, init, speed, view)

import Direction exposing (Direction(..))
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Screen exposing (spotSize)
import Utils exposing (px)


type alias Player =
    { x : Int
    , y : Int
    , direction : Direction
    }


init : Player
init =
    { x = 7 * spotSize
    , y = 11 * spotSize
    , direction = Left
    }


view : Player -> Html msg
view player =
    div
        [ style "position" "absolute"
        , style "background-color" color
        , style "width" (px spotSize)
        , style "height" (px spotSize)
        , style "left" (px player.x)
        , style "top" (px player.y)
        ]
        []


speed : Int
speed =
    2



-- INTERNAL


color : String
color =
    "#1678aa"
