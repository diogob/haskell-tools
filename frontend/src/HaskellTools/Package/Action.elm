module HaskellTools.Package.Action (..) where

import HaskellTools.Package.Model exposing (..)
import Http

type Action
  = FetchPackages (Result Http.Error (List Model))
  | SearchPackages (Result Http.Error (List Model))
  | TaskDone ()
