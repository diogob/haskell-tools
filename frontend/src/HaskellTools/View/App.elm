module HaskellTools.View.App (..) where

import HaskellTools.Action exposing (..)
import HaskellTools.Model exposing (AppModel)
import HaskellTools.Package.Views.ListItem as Package
import HaskellTools.Package.Views.SearchBox as Search
import HaskellTools.View.Head as Head
import Html exposing (Html, ul, div, main')
import Html.Attributes exposing (class)
import Signal


view : Signal.Address Action -> AppModel -> Html
view address model =
  div
    []
    [ Head.view
    , main'
        [ class "app-body" ]
        [ Search.view
        , ul
          []
          (List.map (Package.view address) model.packages)
        ]
    ]
