module HaskellTools.View (..) where

import HaskellTools.Action exposing (..)
import HaskellTools.Model exposing (Model)
import HaskellTools.Repo.View as Repo
import Html exposing (Html, ol)
import Signal


view : Signal.Address Action -> Model -> Html
view address model =
  ol
    []
    (List.map (Repo.view address) model)
