module Screen exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Utils exposing (px)


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


isScreenSpace : { x : Int, y : Int } -> Bool
isScreenSpace spot =
    List.any ((==) spot) screenSpaces


wallSpotColor : String
wallSpotColor =
    "#1b1bb5"


spaceSpotColor : String
spaceSpotColor =
    "#222"
