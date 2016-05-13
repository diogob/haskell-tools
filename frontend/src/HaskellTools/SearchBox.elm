module HaskellTools.SearchBox exposing (..)

import Html exposing (Html, input, div)
import Html.Attributes exposing (..)

view : Html msg
view =
   div []
    [ input
        [ placeholder "Search package"
        ]
        []
    ]
