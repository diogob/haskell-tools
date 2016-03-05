module Main where

import HaskellTools
import Data.List (intercalate)
import Pipes

main :: IO ()
main = runEffect loop

loop :: Effect IO ()
loop = for (haskellRepos 1) (lift . print)
