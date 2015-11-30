module Main where

import GithubHaskell
import Data.List (intercalate)

main :: IO ()
main = do
  repos <- haskellRepos
  putStrLn $ either (("Error: "++) . show)
                    show
                    repos
