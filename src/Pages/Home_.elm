module Pages.Home_ exposing (Model, Msg, page)

import Array exposing (Array)
import Browser.Events exposing (onKeyDown, onKeyUp)
import Direction exposing (Direction(..))
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Http
import Json.Decode as Decode exposing (Decoder)
import Kanji exposing (Kanji, fetchKanjis, initKanjis, parseKanjiText)
import Monsters exposing (Monster, getTargetMonster, initMonsters, monsterSpeed, replaceMonster, viewMonsters)
import Page exposing (Page)
import Player exposing (Player, initPlayer, playerSpeed, viewPlayer)
import Random
import Route exposing (Route)
import Screen exposing (isScreenSpace, screenSize, screenSpaces, screenWalls, spotSize, viewScreen)
import Set exposing (Set)
import Shared
import Time
import Utils exposing (get, px)
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
    , kanjis : Array Kanji
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { player = initPlayer
      , monsters = initMonsters
      , keysDown = Set.empty
      , kanjis = initKanjis
      }
    , fetchKanjis SetKanjis
    )



-- UPDATE


type Msg
    = UpdateLoop
    | SetKeyDown String
    | SetKeyUp String
    | GenerateMonsterPath Monster
    | SetKanjis (Result Http.Error String)


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

        GenerateMonsterPath newMonster ->
            ( { model | monsters = List.map (replaceMonster newMonster) model.monsters }
            , Effect.none
            )

        SetKanjis (Err _) ->
            ( model, Effect.none )

        SetKanjis (Ok kanjiText) ->
            ( { model | kanjis = parseKanjiText kanjiText }
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
            Direction.fromKeysDown keysDown
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
                        |> List.map (updateMonster model.kanjis targetMonster model.player)
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


updateMonster : Array Kanji -> Monster -> Player -> Monster -> ( Maybe Monster, Effect Msg )
updateMonster kanjis targetMonster player monster =
    if targetMonster == monster && isOverlapping player monster then
        let
            newId =
                monster.id + List.length initMonsters
        in
        case Array.get newId kanjis of
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
            Direction.toDeltas direction
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
                    newPath =
                        case get i screenSpaces of
                            Nothing ->
                                []

                            Just newDestination ->
                                findShortestPath
                                    { x = monster.x, y = monster.y }
                                    newDestination
                in
                { monster | path = newPath }
            )
        |> Random.generate GenerateMonsterPath
        |> Effect.sendCmd


findShortestPath : { x : Int, y : Int } -> { x : Int, y : Int } -> List { x : Int, y : Int }
findShortestPath from to =
    findShortestPath_ from to Set.empty []
        |> Maybe.withDefault []


findShortestPath_ : { x : Int, y : Int } -> { x : Int, y : Int } -> Set ( Int, Int ) -> List { x : Int, y : Int } -> Maybe (List { x : Int, y : Int })
findShortestPath_ from to visited path =
    if Set.member ( from.x, from.y ) visited || not (isScreenSpace from) then
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
                            (Set.insert ( from.x, from.y ) visited)
                            (from :: path)

                    Just resultPath ->
                        Just resultPath
            )
            Nothing
            [ Up, Down, Left, Right ]



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
            , style "flex-direction" "column"
            , style "gap" "8px"
            , style "align-items" "center"
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
            , case getTargetMonster model.monsters of
                Nothing ->
                    Html.text ""

                Just targetMonster ->
                    Html.text ("Eat the kanji named \"" ++ targetMonster.kanji.meaning ++ "\"")
            ]
        ]
    }
