module Pages.Home_ exposing (Model, Msg, page)

import Array exposing (Array)
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes exposing (style)
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
    { title = "Elm Kanji Pac-Man"
    , body =
        [ Html.div
            [ style "min-height" "100vh"
            , style "background-color" "black"
            , style "display" "flex"
            , style "justify-content" "center"
            ]
            [ viewScreen ]
        ]
    }



-- SCREEN


screenSize : Int
screenSize =
    300


spotSize : Int
spotSize =
    20


screen : List (List String)
screen =
    [ "###############"
    , "#      #      #"
    , "# #### # #### #"
    , "#             #"
    , "## ## # # ## ##"
    , "#             #"
    , "# ### # # ### #"
    , "#             #"
    , "# #### # #### #"
    , "#    #   #    #"
    , "## # ## ## # ##"
    , "#  #       #  #"
    , "# #### # #### #"
    , "#             #"
    , "###############"
    ]
        |> List.map (String.split "")


spaceSpot : String
spaceSpot =
    " "


wallSpot : String
wallSpot =
    "#"


screenSpots : String -> List { x : Int, y : Int }
screenSpots spot =
    screen
        |> indexedConcatMap
            (\y row ->
                row
                    |> indexedConcatMap
                        (\x screenSpot ->
                            if screenSpot == spot then
                                [ { x = x, y = y } ]

                            else
                                []
                        )
            )


screenSpaces : List { x : Int, y : Int }
screenSpaces =
    screenSpots spaceSpot


screenWalls : List { x : Int, y : Int }
screenWalls =
    screenSpots wallSpot


indexedConcatMap : (Int -> a -> List b) -> List a -> List b
indexedConcatMap fn list =
    list |> List.indexedMap fn |> List.concat


viewScreen : Html msg
viewScreen =
    Html.div [] (List.map viewScreenRow screen)


viewScreenRow : List String -> Html msg
viewScreenRow screenRow =
    Html.div [ style "display" "flex" ]
        (List.map viewScreenSpot screenRow)


viewScreenSpot : String -> Html msg
viewScreenSpot screenSpot =
    case screenSpot of
        "#" ->
            Html.div
                [ style "width" (px spotSize)
                , style "height" (px spotSize)
                , style "background-color" wallSpotColor
                ]
                []

        " " ->
            Html.div
                [ style "width" (px spotSize)
                , style "height" (px spotSize)
                , style "background-color" spaceSpotColor
                ]
                []

        _ ->
            Html.text ""


wallSpotColor : String
wallSpotColor =
    "#1b1bb5"


spaceSpotColor : String
spaceSpotColor =
    "#000"


px : Int -> String
px x =
    String.fromInt x ++ "px"
