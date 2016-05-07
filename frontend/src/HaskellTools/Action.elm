module HaskellTools.Action (..) where

import HaskellTools.Package.Action as Package

type Action
  = PackageAction Package.Action
  | NoOp
  | ShowError String
