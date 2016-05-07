module HaskellTools.Package.Update (..) where

import Effects exposing (Effects)

import HaskellTools.Package.Model exposing (Model)
import HaskellTools.Package.Action exposing (..)

type alias UpdateModel =
  { packages : List Model
  , showErrorAddress : Signal.Address String
  }

update : Action -> UpdateModel -> ( List Model, Effects Action )
update action model =
  case action of
    TaskDone () ->
      ( model.packages, Effects.none )
    SearchPackages result ->
      ( model.packages, Effects.none )
    FetchPackages result ->
      case result of
        Ok pkgs ->
          ( pkgs, Effects.none )

        Err error ->
          let
            errorMessage =
              toString error
            fx =
              Signal.send model.showErrorAddress errorMessage
                |> Effects.task
                |> Effects.map TaskDone
          in
            ( model.packages, fx )
