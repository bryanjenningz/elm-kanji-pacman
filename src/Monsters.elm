module Monsters exposing (Monster, getTarget, init, replace, speed, view)

import Array
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Kanji exposing (Kanji)
import Screen exposing (spotSize)
import Utils exposing (px)


type alias Monster =
    { id : Int
    , x : Int
    , y : Int
    , kanji : Kanji
    , path : List { x : Int, y : Int }
    }


init : List Monster
init =
    [ { x = 5, y = 3 }
    , { x = 9, y = 3 }
    , { x = 5, y = 7 }
    , { x = 9, y = 7 }
    ]
        |> List.indexedMap
            (\i { x, y } ->
                { id = i
                , x = x * spotSize
                , y = y * spotSize
                , kanji =
                    Array.get i Kanji.init
                        |> Maybe.withDefault Kanji.default
                , path = []
                }
            )


view : List Monster -> Html msg
view monsters =
    div [] (List.map viewMonster monsters)


replace : Monster -> Monster -> Monster
replace newMonster monster =
    if monster.id == newMonster.id then
        newMonster

    else
        monster


getTarget : List Monster -> Maybe Monster
getTarget monsters =
    List.sortBy .id monsters |> List.head


speed : Int
speed =
    1



-- INTERNAL


viewMonster : Monster -> Html msg
viewMonster monster =
    div
        [ style "width" (px spotSize)
        , style "height" (px spotSize)
        , style "background-color" color
        , style "position" "absolute"
        , style "left" (px monster.x)
        , style "top" (px monster.y)
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ text monster.kanji.character ]


color : String
color =
    "#000"
