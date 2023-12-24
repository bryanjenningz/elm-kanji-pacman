module Direction exposing (Direction(..), fromArrowKey, toArrowKey, toDeltas, toString)

import Set exposing (Set)


type Direction
    = Up
    | Down
    | Left
    | Right


fromArrowKey : String -> Maybe Direction
fromArrowKey key =
    if key == "ArrowUp" then
        Just Up

    else if key == "ArrowDown" then
        Just Down

    else if key == "ArrowLeft" then
        Just Left

    else if key == "ArrowRight" then
        Just Right

    else
        Nothing


toArrowKey : Direction -> String
toArrowKey direction =
    case direction of
        Up ->
            "ArrowUp"

        Down ->
            "ArrowDown"

        Left ->
            "ArrowLeft"

        Right ->
            "ArrowRight"


toDeltas : Direction -> { dx : Int, dy : Int }
toDeltas direction =
    case direction of
        Up ->
            { dx = 0, dy = -1 }

        Down ->
            { dx = 0, dy = 1 }

        Left ->
            { dx = -1, dy = 0 }

        Right ->
            { dx = 1, dy = 0 }


toString : Direction -> String
toString direction =
    case direction of
        Up ->
            "up"

        Down ->
            "down"

        Left ->
            "left"

        Right ->
            "right"
