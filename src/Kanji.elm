module Kanji exposing (..)

import Array exposing (Array)
import Effect exposing (Effect)
import Http


type alias Kanji =
    { character : String
    , meaning : String
    }


defaultKanji : Kanji
defaultKanji =
    { character = "一", meaning = "one" }


initKanjis : Array Kanji
initKanjis =
    [ defaultKanji
    , { character = "二", meaning = "two" }
    , { character = "三", meaning = "three" }
    , { character = "四", meaning = "four" }
    , { character = "五", meaning = "five" }
    , { character = "六", meaning = "six" }
    , { character = "七", meaning = "seven" }
    , { character = "八", meaning = "eight" }
    , { character = "九", meaning = "nine" }
    , { character = "十", meaning = "ten" }
    ]
        |> Array.fromList


fetchKanjis : (Result Http.Error String -> msg) -> Effect msg
fetchKanjis toMsg =
    Http.get { url = "/heisig-kanji.txt", expect = Http.expectString toMsg }
        |> Effect.sendCmd


parseKanjiText : String -> Array Kanji
parseKanjiText kanjiText =
    kanjiText
        |> String.split "\n"
        |> List.filterMap
            (\line ->
                case String.split "\t" line of
                    [ character, meaning ] ->
                        Just
                            { character = character
                            , meaning = meaning
                            }

                    _ ->
                        Nothing
            )
        |> Array.fromList
