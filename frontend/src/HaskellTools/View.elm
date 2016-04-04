module HaskellTools.View (..) where

import HaskellTools.Action exposing (..)
import HaskellTools.Model exposing (Model, Repo)
import Html exposing (Html, ol, li, div, button, text, a)
import Html.Attributes exposing (href, target)
import Signal


view : Signal.Address Action -> Model -> Html
view address model =
  ol
    []
    (List.map (repoView address) model)


repoView : Signal.Address Action -> Repo -> Html
repoView address model =
  li
    []
    [ a
        [ href model.url, target "blank" ]
        [ text <| model.owner ++ " / " ++ model.name ]
    ]
