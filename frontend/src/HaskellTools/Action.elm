module HaskellTools.Action (..) where

import HaskellTools.Model exposing (Model)


type Action
  = RequestMore
  | NewRepos (Maybe Model)
