module Screen exposing (isSpace, size, spaces, spotSize, view, walls)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Utils exposing (px)


size : Int
size =
    300


spotSize : Int
spotSize =
    20


spaces : List { x : Int, y : Int }
spaces =
    screenSpots spaceSpot


walls : List { x : Int, y : Int }
walls =
    screenSpots wallSpot


view : Html msg
view =
    div [] (List.map viewScreenRow screen)


isSpace : { x : Int, y : Int } -> Bool
isSpace spot =
    List.any ((==) spot) spaces



-- INTERNAL


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


spaceSpot : String
spaceSpot =
    " "


wallSpot : String
wallSpot =
    "#"


wallSpotColor : String
wallSpotColor =
    "#1b1bb5"


spaceSpotColor : String
spaceSpotColor =
    "#222"


indexedConcatMap : (Int -> a -> List b) -> List a -> List b
indexedConcatMap fn list =
    list |> List.indexedMap fn |> List.concat


viewScreenRow : List String -> Html msg
viewScreenRow screenRow =
    div [ style "display" "flex" ]
        (List.map viewScreenSpot screenRow)


viewScreenSpot : String -> Html msg
viewScreenSpot screenSpot =
    case screenSpot of
        "#" ->
            div
                [ style "width" (px spotSize)
                , style "height" (px spotSize)
                , style "background-color" wallSpotColor
                ]
                []

        " " ->
            div
                [ style "width" (px spotSize)
                , style "height" (px spotSize)
                , style "background-color" spaceSpotColor
                ]
                []

        _ ->
            text ""
