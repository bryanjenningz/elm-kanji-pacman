module Monsters exposing (..)

import Array
import Html exposing (Html)
import Html.Attributes exposing (style)
import Kanji exposing (Kanji, defaultKanji, initKanjis)
import Screen exposing (spotSize)
import Utils exposing (px)


type alias Monster =
    { id : Int
    , x : Int
    , y : Int
    , kanji : Kanji
    , path : List { x : Int, y : Int }
    }


initMonsters : List Monster
initMonsters =
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
                    Array.get i initKanjis
                        |> Maybe.withDefault defaultKanji
                , path = []
                }
            )


viewMonsters : List Monster -> Html msg
viewMonsters monsters =
    Html.div [] (List.map viewMonster monsters)


viewMonster : Monster -> Html msg
viewMonster monster =
    Html.div
        [ style "width" (px spotSize)
        , style "height" (px spotSize)
        , style "background-color" monsterColor
        , style "position" "absolute"
        , style "left" (px monster.x)
        , style "top" (px monster.y)
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Html.text monster.kanji.character ]


monsterColor : String
monsterColor =
    "#000"


replaceMonster : Monster -> Monster -> Monster
replaceMonster newMonster monster =
    if monster.id == newMonster.id then
        newMonster

    else
        monster


getTargetMonster : List Monster -> Maybe Monster
getTargetMonster monsters =
    List.sortBy .id monsters |> List.head


monsterSpeed : Int
monsterSpeed =
    1
