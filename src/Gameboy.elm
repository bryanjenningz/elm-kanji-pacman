module Gameboy exposing (view)

import Direction exposing (Direction(..))
import Html exposing (Attribute, Html, button, div)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick, onMouseDown, onMouseUp)
import Json.Decode as Decode exposing (Decoder)
import Position exposing (Position)


onTouchStart : (Maybe Direction -> msg) -> Attribute msg
onTouchStart toMsg =
    on "touchstart" (Decode.map toMsg touchDirectionDecoder)


onTouchEnd : msg -> Attribute msg
onTouchEnd msg =
    on "touchend" (Decode.succeed msg)


onTouchMove : (Maybe Direction -> msg) -> Attribute msg
onTouchMove toMsg =
    on "touchmove" (Decode.map toMsg touchDirectionDecoder)


touchDirectionDecoder : Decoder (Maybe Direction)
touchDirectionDecoder =
    Decode.map2 (\x y -> padPositionToDirection { x = x, y = y })
        clientXDecoder
        clientYDecoder


clientXDecoder : Decoder Int
clientXDecoder =
    Decode.at [ "targetTouches", "0", "clientX" ] Decode.float
        |> Decode.map round


clientYDecoder : Decoder Int
clientYDecoder =
    Decode.at [ "targetTouches", "0", "clientY" ] Decode.float
        |> Decode.map round


type alias Rectangle =
    { position : Position
    , size : Size
    }


type alias Size =
    { width : Int
    , height : Int
    }


buttonPadSize : Size
buttonPadSize =
    { width = 50
    , height = 50
    }



-- TODO: Remove hardcoded values button positions, instead calculate them after
-- the buttons get rendered to the DOM.


upButtonRectangle : Rectangle
upButtonRectangle =
    { position = { x = 80, y = 280 }
    , size = buttonPadSize
    }


downButtonRectangle : Rectangle
downButtonRectangle =
    { position = { x = 80, y = 360 }
    , size = buttonPadSize
    }


leftButtonRectangle : Rectangle
leftButtonRectangle =
    { position = { x = 40, y = 320 }
    , size = buttonPadSize
    }


rightButtonRectangle : Rectangle
rightButtonRectangle =
    { position = { x = 120, y = 320 }
    , size = buttonPadSize
    }


isWithin : Position -> Rectangle -> Bool
isWithin position rect =
    (position.x >= rect.position.x)
        && (position.x <= rect.position.x + rect.size.width)
        && (position.y >= rect.position.y)
        && (position.y <= rect.position.y + rect.size.height)


padPositionToDirection : Position -> Maybe Direction
padPositionToDirection position =
    if isWithin position upButtonRectangle then
        Just Up

    else if isWithin position downButtonRectangle then
        Just Down

    else if isWithin position leftButtonRectangle then
        Just Left

    else if isWithin position rightButtonRectangle then
        Just Right

    else
        Nothing


type alias Controls msg =
    { onPadDown : Direction -> msg
    , onPadUp : Direction -> msg
    , onAllPadsUp : msg
    , onClickA : msg
    , onClickB : msg
    , onClickStart : msg
    , onClickSelect : msg
    }


view : Controls msg -> Html msg -> Html msg
view controls screen =
    div
        [ style "width" "350px"
        , style "height" "500px"
        , style "background-color" "#111164"
        , style "border-radius" "16px"
        , style "padding" "40px"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        ]
        [ viewTop screen
        , viewBottom controls
        ]


viewTop : Html msg -> Html msg
viewTop screen =
    div
        [ style "width" "280px"
        , style "height" "230px"
        , style "background-color" "black"
        , style "border-radius" "16px"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ div
            [ style "width" "200px"
            , style "height" "200px"
            , style "background-color" "#333"
            , style "border-radius" "16px"
            , style "position" "relative"
            , style "overflow" "hidden"
            ]
            [ screen ]
        ]


viewBottom : Controls msg -> Html msg
viewBottom controls =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "16px"
        , style "width" "100%"
        ]
        [ div
            [ style "display" "flex"
            , style "justify-content" "space-between"
            , style "padding" "32px 0"
            , style "width" "100%"
            ]
            [ viewDirectionPad controls
            , viewABButtons controls
            ]
        , viewStartSelectButtons controls
        ]


viewDirectionPad : Controls msg -> Html msg
viewDirectionPad controls =
    div
        [ style "position" "relative"
        , style "width" "100px"
        , style "height" "100px"
        ]
        [ div
            [ style "position" "absolute"
            , style "top" "30px"
            , style "width" "100px"
            , style "height" "40px"
            , style "background-color" "black"
            , style "border-radius" "99px"
            , style "overflow" "hidden"
            ]
            [ button
                [ style "position" "absolute"
                , style "top" "0"
                , style "bottom" "0"
                , style "left" "0"
                , style "width" "40px"
                , onMouseDown (controls.onPadDown Left)
                , onMouseUp (controls.onPadUp Left)
                ]
                []
            , button
                [ style "position" "absolute"
                , style "top" "0"
                , style "bottom" "0"
                , style "right" "0"
                , style "width" "40px"
                , onMouseDown (controls.onPadDown Right)
                , onMouseUp (controls.onPadUp Right)
                ]
                []
            ]
        , div
            [ style "position" "absolute"
            , style "left" "30px"
            , style "width" "40px"
            , style "height" "100px"
            , style "background-color" "black"
            , style "border-radius" "99px"
            , style "overflow" "hidden"
            ]
            [ button
                [ style "position" "absolute"
                , style "top" "0"
                , style "left" "0"
                , style "right" "0"
                , style "height" "40px"
                , onMouseDown (controls.onPadDown Up)
                , onMouseUp (controls.onPadUp Up)
                ]
                []
            , button
                [ style "position" "absolute"
                , style "bottom" "0"
                , style "left" "0"
                , style "right" "0"
                , style "height" "40px"
                , onMouseDown (controls.onPadDown Down)
                , onMouseUp (controls.onPadUp Down)
                ]
                []
            ]

        -- MOBILE TOUCH PAD OVERLAY
        -- We need to use a single button instead of 4 buttons so
        -- that we can handle touchmove events that start in one
        -- direction and change to a different direction
        -- (e.g. the user touches the UP button then slides their
        -- finger to the LEFT button without lifting their finger)
        , button
            [ style "position" "absolute"
            , style "inset" "0"
            , style "z-index" "1"
            , onTouchStart
                (\maybeDirection ->
                    case maybeDirection of
                        Nothing ->
                            controls.onAllPadsUp

                        Just direction ->
                            controls.onPadDown direction
                )
            , onTouchMove
                (\maybeDirection ->
                    case maybeDirection of
                        Nothing ->
                            controls.onAllPadsUp

                        Just direction ->
                            controls.onPadDown direction
                )
            , onTouchEnd controls.onAllPadsUp
            ]
            []
        ]


viewABButtons : Controls msg -> Html msg
viewABButtons controls =
    div
        [ style "position" "relative"
        , style "width" "100px"
        , style "height" "100px"
        ]
        [ button
            [ style "position" "absolute"
            , style "top" "45px"
            , style "width" "50px"
            , style "height" "50px"
            , style "background-color" "black"
            , style "border-radius" "99px"
            , onClick controls.onClickB
            ]
            []
        , button
            [ style "position" "absolute"
            , style "top" "10px"
            , style "right" "0px"
            , style "width" "50px"
            , style "height" "50px"
            , style "background-color" "black"
            , style "border-radius" "99px"
            , onClick controls.onClickA
            ]
            []
        ]


viewStartSelectButtons : Controls msg -> Html msg
viewStartSelectButtons controls =
    div
        [ style "width" "100%"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "gap" "20px"
        ]
        [ button
            [ style "width" "40px"
            , style "height" "15px"
            , style "background-color" "black"
            , style "border-radius" "20px"
            , onClick controls.onClickSelect
            ]
            []
        , button
            [ style "width" "40px"
            , style "height" "15px"
            , style "background-color" "black"
            , style "border-radius" "20px"
            , onClick controls.onClickStart
            ]
            []
        ]
