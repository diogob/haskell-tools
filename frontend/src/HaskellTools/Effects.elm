module HaskellTools.Effects (..) where

import Effects exposing (Effects, Never)
import HaskellTools.Model exposing (Model, decodeModel)
import HaskellTools.Action exposing (..)
import Http
import Task


packagesUrl : String
packagesUrl =
  "http://localhost:3000/packages"


getPackages : Effects Action
getPackages =
  Http.get decodeModel packagesUrl
    |> Task.toMaybe
    |> Task.map NewPackages
    |> Effects.task
