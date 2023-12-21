module Gameboy exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (style)


view : Html msg
view =
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
                ]
                []
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
                        , style "top" "35px"
                        , style "width" "100px"
                        , style "height" "30px"
                        , style "background-color" "black"
                        , style "border-radius" "16px"
                        ]
                        []
                    , div
                        [ style "position" "absolute"
                        , style "left" "35px"
                        , style "width" "30px"
                        , style "height" "100px"
                        , style "background-color" "black"
                        , style "border-radius" "16px"
                        ]
                        []
                    ]

                -- GAMEBOY RIGHT BUTTONS
                , div
                    [ style "position" "relative"
                    , style "width" "100px"
                    , style "height" "100px"
                    ]
                    [ div
                        [ style "position" "absolute"
                        , style "top" "45px"
                        , style "width" "45px"
                        , style "height" "45px"
                        , style "background-color" "black"
                        , style "border-radius" "40px"
                        ]
                        []
                    , div
                        [ style "position" "absolute"
                        , style "top" "10px"
                        , style "right" "0px"
                        , style "width" "45px"
                        , style "height" "45px"
                        , style "background-color" "black"
                        , style "border-radius" "40px"
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
                [ div
                    [ style "width" "40px"
                    , style "height" "15px"
                    , style "background-color" "black"
                    , style "border-radius" "20px"
                    ]
                    []
                , div
                    [ style "width" "40px"
                    , style "height" "15px"
                    , style "background-color" "black"
                    , style "border-radius" "20px"
                    ]
                    []
                ]
            ]
        ]
