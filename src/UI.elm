module UI exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


hSpacer : List (Html msg) -> Html msg
hSpacer =
    div
        [ style "display" "flex"
        , style "gap" "18px"
        ]


vSpacer : List (Html msg) -> Html msg
vSpacer =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "stretch"
        , style "gap" "16px"
        ]


viewIcon : Html msg
viewIcon =
    div
        [ style "display" "inline-block"
        , style "width" "20px"
        , style "height" "20px"
        , style "border-radius" "100%"
        , style "border" "2px solid currentColor"
        ]
        []
