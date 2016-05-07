module HaskellTools.Model (..) where

import HaskellTools.Package.Model as Package

type alias AppModel =
  { packages : List Package.Model
  , errorMessage : String
  }
