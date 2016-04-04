module HaskellTools.Update (..) where

import Effects exposing (Effects)
import HaskellTools.Action exposing (..)
import HaskellTools.Effects exposing (getTopRepos)
import HaskellTools.Model exposing (Model)


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    RequestMore ->
      ( model, getTopRepos )

    NewRepos maybeRepos ->
      ( Maybe.withDefault [] maybeRepos
      , Effects.none
      )
