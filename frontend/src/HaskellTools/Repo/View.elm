module HaskellTools.Repo.View (..) where

import HaskellTools.Action exposing (..)
import HaskellTools.Repo.Model as Repo
import Html exposing (Html, li, text, a)
import Html.Attributes exposing (href, target)
import Signal


view : Signal.Address Action -> Repo.Model -> Html
view address model =
  li
    []
    [ a
        [ href model.url, target "blank" ]
        [ text <| model.owner ++ " / " ++ model.name ]
    ]
