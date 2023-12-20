module Pages.Home_ exposing (Model, Msg, page)

import Array exposing (Array)
import Effect exposing (Effect)
import Html
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { player : Player
    , monsters : List Monster
    }



-- PLAYER


type alias Player =
    { x : Int
    , y : Int
    , direction : Direction
    }


type Direction
    = Up
    | Down
    | Left
    | Right


initPlayer : Player
initPlayer =
    { x = 7
    , y = 10
    , direction = Left
    }



-- MONSTER


type alias Monster =
    { id : Int
    , x : Int
    , y : Int
    , kanji : Kanji
    , path : List { x : Int, y : Int }
    }


initMonsters : List Monster
initMonsters =
    [ { x = 4, y = 4 }
    , { x = 8, y = 4 }
    , { x = 4, y = 8 }
    , { x = 8, y = 8 }
    ]
        |> List.indexedMap
            (\i { x, y } ->
                { id = i
                , x = x
                , y = y
                , kanji =
                    Array.get i initKanjis
                        |> Maybe.withDefault defaultKanji
                , path = []
                }
            )



-- KANJI


type alias Kanji =
    { name : String
    , meaning : String
    }


defaultKanji : Kanji
defaultKanji =
    { name = "", meaning = "one" }


initKanjis : Array Kanji
initKanjis =
    [ defaultKanji
    , { name = "", meaning = "two" }
    , { name = "", meaning = "three" }
    , { name = "", meaning = "four" }
    , { name = "", meaning = "five" }
    , { name = "", meaning = "six" }
    , { name = "", meaning = "seven" }
    , { name = "", meaning = "eight" }
    , { name = "", meaning = "nine" }
    , { name = "", meaning = "ten" }
    ]
        |> Array.fromList


init : () -> ( Model, Effect Msg )
init () =
    ( { player = initPlayer
      , monsters = initMonsters
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.Home_"
    , body = [ Html.text "/" ]
    }
