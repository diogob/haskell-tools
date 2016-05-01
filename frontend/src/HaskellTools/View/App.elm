module HaskellTools.View.App (..) where

import HaskellTools.Action exposing (..)
import HaskellTools.Model exposing (Model)
import HaskellTools.Package.View as Package
import HaskellTools.View.Head as Head
import Html exposing (Html, ol, div, main')
import Html.Attributes exposing (class)
import Signal


view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    [ Head.view
    , main'
        [ class "app-body" ]
        [ ol
            []
            (List.map (Package.view address) model)
        ]
    ]
