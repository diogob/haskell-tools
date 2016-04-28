module HaskellTools.Types where

import Data.Vector (Vector)
import qualified Data.Text as T

data Repo = Repo
  { repoPackageName :: T.Text
  , stars :: Integer
  , forks :: Integer
  , collaborators :: Integer
  } deriving (Show, Eq)

type PackageRepos = (Vector (T.Text, T.Text, T.Text))
