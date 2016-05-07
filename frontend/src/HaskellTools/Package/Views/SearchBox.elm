module HaskellTools.Package.Views.SearchBox (..) where

import Html exposing (Html, input, div)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)

import Signal

view : Html
view =
   div []
    [ input
        [ placeholder "Search package"
        ]
        []
    ]
