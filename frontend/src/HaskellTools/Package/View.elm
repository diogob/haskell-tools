module HaskellTools.Package.View (..) where

import HaskellTools.Action exposing (..)
import HaskellTools.Package.Model exposing (Model)
import Html exposing (Html, li, text, a)
import Html.Attributes exposing (href, target)
import Signal


view : Signal.Address Action -> Model -> Html
view address model =
  li
    []
    [ a
        [ href model.package_url, target "blank" ]
        [ text <| model.package_name ]
    ]
