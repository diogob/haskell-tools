module HaskellTools.Effects (..) where

import Effects exposing (Effects, Never)
import HaskellTools.Model exposing (Model, decodeRepos, topReposUrl)
import HaskellTools.Action exposing (..)
import Http
import Task


getTopRepos : Effects Action
getTopRepos =
  Http.get decodeRepos topReposUrl
    |> Task.toMaybe
    |> Task.map NewRepos
    |> Effects.task
