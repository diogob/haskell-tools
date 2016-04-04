module HaskellTools.Action (..) where

import HaskellTools.Model exposing (Model)


type Action
  = NewRepos (Maybe Model)
