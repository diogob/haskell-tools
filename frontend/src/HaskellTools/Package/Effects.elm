module HaskellTools.Package.Effects (..) where

import Effects exposing (Effects, Never)
import HaskellTools.Package.Model exposing (..)
import HaskellTools.Package.Action exposing (..)
import HaskellTools.Action as AppAction
import Http
import Task

packagesUrl : String
packagesUrl =
  "http://localhost:3000/packages"

getPackages : Effects AppAction.Action
getPackages =
  Http.get decodeModel packagesUrl
    |> Task.toResult
    |> Task.map (AppAction.PackageAction << FetchPackages)
    |> Effects.task
