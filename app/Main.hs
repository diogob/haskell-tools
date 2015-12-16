module Main where

import HaskellTools
import Data.List (intercalate)

main :: IO ()
main = do
  repos <- haskellRepos 1
  print repos
