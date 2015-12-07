module Main where

import GithubHaskell

main :: IO ()
main = do
  repos <- haskellRepos 1
  print repos
