module HaskellTools.Search exposing (Msg(..), view)

import Html exposing (Html, input, div)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type Msg =
  SearchPackages String

view : Html Msg
view =
   div [ class "full" ]
    [ input
        [ placeholder "Search package"
        , onInput SearchPackages
        ]
        []
    ]
