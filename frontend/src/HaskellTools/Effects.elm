module HaskellTools.Effects (..) where

import Effects exposing (Effects, Never)
import HaskellTools.Model exposing (Model, decodeRepos)
import HaskellTools.Action exposing (..)
import Http
import Task


topReposUrl : String
topReposUrl =
  "http://localhost:3000/top_repos"


getTopRepos : Effects Action
getTopRepos =
  Http.get decodeRepos topReposUrl
    |> Task.toMaybe
    |> Task.map NewRepos
    |> Effects.task
