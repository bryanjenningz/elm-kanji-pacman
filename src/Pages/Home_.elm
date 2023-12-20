module Pages.Home_ exposing (Model, Msg, page)

import Array exposing (Array)
import Browser.Events exposing (onKeyDown, onKeyUp)
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Json.Decode as Decode exposing (Decoder)
import Page exposing (Page)
import Random
import Route exposing (Route)
import Set exposing (Set)
import Shared
import Time
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
    , keysDown : Set String
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
    { x = 7 * spotSize
    , y = 11 * spotSize
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
    [ { x = 5, y = 3 }
    , { x = 9, y = 3 }
    , { x = 5, y = 7 }
    , { x = 9, y = 7 }
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
      , keysDown = Set.empty
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = UpdateLoop
    | SetKeyDown String
    | SetKeyUp String
    | GenerateMonsterPath Monster


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        UpdateLoop ->
            updateLoop model

        SetKeyDown key ->
            ( { model | keysDown = Set.insert key model.keysDown }
            , Effect.none
            )

        SetKeyUp key ->
            ( { model | keysDown = Set.remove key model.keysDown }
            , Effect.none
            )

        GenerateMonsterPath monster ->
            ( { model
                | monsters =
                    List.map
                        (\m ->
                            if m.id == monster.id then
                                monster

                            else
                                m
                        )
                        model.monsters
              }
            , Effect.none
            )


updateLoop : Model -> ( Model, Effect Msg )
updateLoop model =
    let
        ( newMonsters, newEffect ) =
            updateMonsters model
    in
    ( { model
        | player = updatePlayer model
        , monsters = newMonsters
      }
    , newEffect
    )


updatePlayer : Model -> Player
updatePlayer ({ player, keysDown } as model) =
    let
        newDirection =
            directionFromKeysDown keysDown
                |> Maybe.withDefault player.direction

        playerWithNewDirection =
            if isOverlappingWall (movePlayer { player | direction = newDirection }) then
                player

            else
                { player | direction = newDirection }

        newPlayer =
            if isOverlappingWall (movePlayer playerWithNewDirection) then
                playerWithNewDirection

            else
                movePlayer playerWithNewDirection
    in
    newPlayer


updateMonsters : Model -> ( List Monster, Effect Msg )
updateMonsters model =
    case getTargetMonster model.monsters of
        Nothing ->
            ( model.monsters, Effect.none )

        Just targetMonster ->
            let
                ( newMonsters, newEffects ) =
                    model.monsters
                        |> List.map (updateMonster targetMonster model.player)
                        |> partition
            in
            ( List.filterMap identity newMonsters
            , Effect.batch newEffects
            )


partition : List ( a, b ) -> ( List a, List b )
partition list =
    ( List.map Tuple.first list
    , List.map Tuple.second list
    )


updateMonster : Monster -> Player -> Monster -> ( Maybe Monster, Effect Msg )
updateMonster targetMonster player monster =
    if targetMonster == monster && isOverlapping player monster then
        let
            newId =
                monster.id + List.length initMonsters
        in
        case Array.get newId initKanjis of
            Nothing ->
                ( Nothing, Effect.none )

            Just newKanji ->
                ( Just { monster | id = newId, kanji = newKanji }
                , Effect.none
                )

    else
        case monster.path of
            [] ->
                ( Just monster
                , generateMonsterPath monster
                )

            nextStep :: remPath ->
                if isSamePosition nextStep monster then
                    ( Just { monster | path = remPath }
                    , Effect.none
                    )

                else
                    moveMonster monster
                        |> Tuple.mapFirst Just


getTargetMonster : List Monster -> Maybe Monster
getTargetMonster monsters =
    List.sortBy .id monsters |> List.head


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


isOverlapping : { a | x : Int, y : Int } -> { b | x : Int, y : Int } -> Bool
isOverlapping a b =
    (a.x + spotSize > b.x)
        && (a.x < b.x + spotSize)
        && (a.y + spotSize > b.y)
        && (a.y < b.y + spotSize)


isOverlappingWall : { a | x : Int, y : Int } -> Bool
isOverlappingWall value =
    List.any (isOverlapping value) screenWalls


move : Int -> Direction -> { a | x : Int, y : Int } -> { a | x : Int, y : Int }
move speed direction value =
    let
        { dx, dy } =
            directionDeltas direction
    in
    { value | x = value.x + dx * speed, y = value.y + dy * speed }


movePlayer : Player -> Player
movePlayer player =
    move playerSpeed player.direction player


moveMonster : Monster -> ( Monster, Effect Msg )
moveMonster monster =
    case monsterDirection monster of
        Nothing ->
            ( monster, generateMonsterPath monster )

        Just direction ->
            ( move monsterSpeed direction monster, Effect.none )


monsterDirection : Monster -> Maybe Direction
monsterDirection monster =
    case monster.path of
        [] ->
            Nothing

        nextStep :: remPath ->
            if isSamePosition nextStep monster then
                monsterDirection { monster | path = remPath }

            else
                let
                    dx =
                        nextStep.x - monster.x

                    dy =
                        nextStep.y - monster.y
                in
                if dy < 0 then
                    Just Up

                else if dy > 0 then
                    Just Down

                else if dx < 0 then
                    Just Left

                else if dx > 0 then
                    Just Right

                else
                    Nothing


isSamePosition : { a | x : Int, y : Int } -> { b | x : Int, y : Int } -> Bool
isSamePosition a b =
    a.x == b.x && a.y == b.y


generateMonsterPath : Monster -> Effect Msg
generateMonsterPath monster =
    Random.int 0 (List.length screenSpaces - 1)
        |> Random.map
            (\i ->
                let
                    newDestination =
                        get i screenSpaces
                            |> Maybe.withDefault { x = 20, y = 20 }

                    newPath =
                        findShortestPath { x = monster.x, y = monster.y } newDestination
                in
                { monster | path = newPath }
            )
        |> Random.generate GenerateMonsterPath
        |> Effect.sendCmd


findShortestPath : { x : Int, y : Int } -> { x : Int, y : Int } -> List { x : Int, y : Int }
findShortestPath from to =
    findShortestPath_ from to []
        |> Maybe.withDefault []


findShortestPath_ : { x : Int, y : Int } -> { x : Int, y : Int } -> List { x : Int, y : Int } -> Maybe (List { x : Int, y : Int })
findShortestPath_ from to path =
    if not (isScreenSpace from) then
        Nothing

    else if from == to then
        Just (List.reverse path)

    else
        List.foldl
            (\direction result ->
                case result of
                    Nothing ->
                        findShortestPath_
                            (move spotSize direction from)
                            to
                            (from :: path)

                    Just resultPath ->
                        Just resultPath
            )
            Nothing
            [ Up, Down, Left, Right ]


isScreenSpace : { x : Int, y : Int } -> Bool
isScreenSpace spot =
    List.any ((==) spot) screenSpaces


get : Int -> List a -> Maybe a
get i list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if i <= 0 then
                Just first

            else
                get (i - 1) rest


playerSpeed : Int
playerSpeed =
    2


monsterSpeed : Int
monsterSpeed =
    1


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 15 (\_ -> UpdateLoop)
        , onKeyDown (Decode.map SetKeyDown keyDecoder)
        , onKeyUp (Decode.map SetKeyUp keyDecoder)
        ]


keyDecoder : Decoder String
keyDecoder =
    Decode.field "key" Decode.string



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
            [ Html.div
                [ style "width" (px screenSize)
                , style "height" (px screenSize)
                , style "position" "relative"
                ]
                [ viewScreen
                , viewPlayer model.player
                , viewMonsters model.monsters
                ]
            ]
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
                                [ { x = x * spotSize, y = y * spotSize } ]

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
    "#222"


px : Int -> String
px x =
    String.fromInt x ++ "px"



-- PLAYER


viewPlayer : Player -> Html msg
viewPlayer player =
    Html.div
        [ style "position" "absolute"
        , style "background-color" playerColor
        , style "width" (px spotSize)
        , style "height" (px spotSize)
        , style "left" (px player.x)
        , style "top" (px player.y)
        ]
        []


playerColor : String
playerColor =
    "#1678aa"



-- MONSTERS


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
        , style "left" (px (monster.x * spotSize))
        , style "top" (px (monster.y * spotSize))
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Html.text monster.kanji.character ]


monsterColor : String
monsterColor =
    "#000"
