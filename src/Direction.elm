module Direction exposing (..)

import Set exposing (Set)


type Direction
    = Up
    | Down
    | Left
    | Right


directionDeltas : Direction -> { dx : Int, dy : Int }
directionDeltas direction =
    case direction of
        Up ->
            { dx = 0, dy = -1 }

        Down ->
            { dx = 0, dy = 1 }

        Left ->
            { dx = -1, dy = 0 }

        Right ->
            { dx = 1, dy = 0 }


directionFromKeysDown : Set String -> Maybe Direction
directionFromKeysDown keysDown =
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
