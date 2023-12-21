module Pages.Home_ exposing (Model, Msg, page)

import Array exposing (Array)
import Browser.Events exposing (onKeyDown, onKeyUp)
import Direction exposing (Direction(..))
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Http
import Json.Decode as Decode exposing (Decoder)
import Kanji exposing (Kanji)
import Monsters exposing (Monster)
import Page exposing (Page)
import Player exposing (Player)
import Position
import Random
import Route exposing (Route)
import Screen
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
    ( { player = Player.init
      , monsters = Monsters.init
      , keysDown = Set.empty
      , kanjis = Kanji.init
      }
    , Kanji.fetch SetKanjis
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
            ( { model | monsters = List.map (Monsters.replace newMonster) model.monsters }
            , Effect.none
            )

        SetKanjis (Err _) ->
            ( model, Effect.none )

        SetKanjis (Ok kanjiText) ->
            ( { model | kanjis = Kanji.parse kanjiText }
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
            if Position.isOverlappingWall (movePlayer { player | direction = newDirection }) then
                player

            else
                { player | direction = newDirection }

        newPlayer =
            if Position.isOverlappingWall (movePlayer playerWithNewDirection) then
                playerWithNewDirection

            else
                movePlayer playerWithNewDirection
    in
    newPlayer


updateMonsters : Model -> ( List Monster, Effect Msg )
updateMonsters model =
    case Monsters.getTarget model.monsters of
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
    if targetMonster == monster && Position.isOverlapping player monster then
        let
            newId =
                monster.id + List.length Monsters.init
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
                if Position.equals nextStep monster then
                    ( Just { monster | path = remPath }
                    , Effect.none
                    )

                else
                    moveMonster monster
                        |> Tuple.mapFirst Just


movePlayer : Player -> Player
movePlayer player =
    Position.move Player.speed player.direction player


moveMonster : Monster -> ( Monster, Effect Msg )
moveMonster monster =
    case monsterDirection monster of
        Nothing ->
            ( monster, generateMonsterPath monster )

        Just direction ->
            ( Position.move Monsters.speed direction monster, Effect.none )


monsterDirection : Monster -> Maybe Direction
monsterDirection monster =
    case monster.path of
        [] ->
            Nothing

        nextStep :: remPath ->
            if Position.equals nextStep monster then
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


generateMonsterPath : Monster -> Effect Msg
generateMonsterPath monster =
    Random.int 0 (List.length Screen.spaces - 1)
        |> Random.map
            (\i ->
                let
                    newPath =
                        case get i Screen.spaces of
                            Nothing ->
                                []

                            Just newDestination ->
                                Position.shortestPath
                                    { x = monster.x, y = monster.y }
                                    newDestination
                in
                { monster | path = newPath }
            )
        |> Random.generate GenerateMonsterPath
        |> Effect.sendCmd



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
                [ style "width" (px Screen.size)
                , style "height" (px Screen.size)
                , style "position" "relative"
                ]
                [ Screen.view
                , Player.view model.player
                , Monsters.view model.monsters
                ]
            , case Monsters.getTarget model.monsters of
                Nothing ->
                    Html.text ""

                Just targetMonster ->
                    Html.text ("Eat the kanji named \"" ++ targetMonster.kanji.meaning ++ "\"")
            ]
        ]
    }
