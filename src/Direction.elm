module Direction exposing (Direction(..), fromKeysDown, toArrowKey, toDeltas)

import Set exposing (Set)


type Direction
    = Up
    | Down
    | Left
    | Right


fromKeysDown : Set String -> Maybe Direction
fromKeysDown keysDown =
    if Set.member "ArrowUp" keysDown then
        Just Up

    else if Set.member "ArrowDown" keysDown then
        Just Down

    else if Set.member "ArrowLeft" keysDown then
        Just Left

    else if Set.member "ArrowRight" keysDown then
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
