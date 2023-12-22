module Gameboy exposing (view)

import Direction exposing (Direction(..))
import Html exposing (Attribute, Html, button, div)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick, onMouseDown, onMouseUp)
import Json.Decode as Decode


onTouchStart : msg -> Attribute msg
onTouchStart message =
    on "touchstart" (Decode.succeed message)


onTouchEnd : msg -> Attribute msg
onTouchEnd message =
    on "touchend" (Decode.succeed message)


type alias Controls msg =
    { onPadDown : Direction -> msg
    , onPadUp : Direction -> msg
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
        -- GAMEBOY TOP
        [ div
            [ style "width" "280px"
            , style "height" "230px"
            , style "background-color" "black"
            , style "border-radius" "16px"
            , style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            ]
            -- GAMEBOY SCREEN
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

        -- GAMEBOY BOTTOM
        , div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "gap" "16px"
            , style "width" "100%"
            ]
            -- GAMEBOY MAIN CONTROLS
            [ div
                [ style "display" "flex"
                , style "justify-content" "space-between"
                , style "padding" "32px 0"
                , style "width" "100%"
                ]
                -- GAMEBOY LEFT PAD
                [ div
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
                            , onTouchStart (controls.onPadDown Left)
                            , onTouchEnd (controls.onPadUp Left)
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
                            , onTouchStart (controls.onPadDown Right)
                            , onTouchEnd (controls.onPadUp Right)
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
                            , onTouchStart (controls.onPadDown Up)
                            , onTouchEnd (controls.onPadUp Up)
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
                            , onTouchStart (controls.onPadDown Down)
                            , onTouchEnd (controls.onPadUp Down)
                            ]
                            []
                        ]
                    ]

                -- GAMEBOY RIGHT BUTTONS
                , div
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
                ]

            -- GAMEBOY START / SELECT
            , div
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
            ]
        ]
