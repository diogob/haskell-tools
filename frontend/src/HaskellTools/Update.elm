module HaskellTools.Update (..) where

import Effects exposing (Effects)
import HaskellTools.Action exposing (..)
import HaskellTools.Model exposing (AppModel)
import HaskellTools.Package.Update as PackageUpdate
import HaskellTools.Mailboxes exposing (..)

update : Action -> AppModel -> ( AppModel, Effects Action )
update action model =
  case (Debug.log "action" action) of
    NoOp ->
      ( model, Effects.none )
    ShowError msg ->
      ( model, Effects.none )
    PackageAction subAction ->
      let
        updateModel =
          { packages = model.packages
          , showErrorAddress = Signal.forwardTo actionsMailbox.address ShowError
          }
        ( updatedPackages, fx ) =
          PackageUpdate.update subAction updateModel
      in
        ( { model | packages = updatedPackages }, Effects.map PackageAction fx )
