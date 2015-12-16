module Main where

import HaskellTools
import Data.List (intercalate)

main :: IO ()
main = do
  repos <- haskellRepos
  putStrLn $ either (("Error: "++) . show)
                    show
                    repos
