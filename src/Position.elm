module Position exposing (equals, isOverlapping, isOverlappingWall, move, shortestPath)

import Direction exposing (Direction(..))
import Screen
import Set exposing (Set)


equals : { a | x : Int, y : Int } -> { b | x : Int, y : Int } -> Bool
equals a b =
    a.x == b.x && a.y == b.y


move : Int -> Direction -> { a | x : Int, y : Int } -> { a | x : Int, y : Int }
move speed direction value =
    let
        { dx, dy } =
            Direction.toDeltas direction
    in
    { value | x = value.x + dx * speed, y = value.y + dy * speed }


shortestPath : { x : Int, y : Int } -> { x : Int, y : Int } -> List { x : Int, y : Int }
shortestPath from to =
    shortestPath_ from to Set.empty []
        |> Maybe.withDefault []


isOverlapping : { a | x : Int, y : Int } -> { b | x : Int, y : Int } -> Bool
isOverlapping a b =
    (a.x + Screen.spotSize > b.x)
        && (a.x < b.x + Screen.spotSize)
        && (a.y + Screen.spotSize > b.y)
        && (a.y < b.y + Screen.spotSize)


isOverlappingWall : { a | x : Int, y : Int } -> Bool
isOverlappingWall value =
    List.any (isOverlapping value) Screen.walls



-- INTERNALS


shortestPath_ : { x : Int, y : Int } -> { x : Int, y : Int } -> Set ( Int, Int ) -> List { x : Int, y : Int } -> Maybe (List { x : Int, y : Int })
shortestPath_ from to visited path =
    if Set.member ( from.x, from.y ) visited || not (Screen.isSpace from) then
        Nothing

    else if from == to then
        Just (List.reverse path)

    else
        List.foldl
            (\direction result ->
                case result of
                    Nothing ->
                        shortestPath_
                            (move Screen.spotSize direction from)
                            to
                            (Set.insert ( from.x, from.y ) visited)
                            (from :: path)

                    Just resultPath ->
                        Just resultPath
            )
            Nothing
            [ Up, Down, Left, Right ]
