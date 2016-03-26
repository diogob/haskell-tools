module Main where

import HaskellTools
import Data.List (intercalate)
import Pipes
import Hasql.Connection

main :: IO ()
main = do
  conOrError <- acquire dbConfig
  case conOrError of
    Left _ -> error "Error connecting"
    Right c -> runEffect $ loop c
  where
    dbConfig = settings "localhost" 5432 "diogo" "" "haskell_tools"

loop :: Connection -> Effect IO ()
loop con = for (haskellRepos 1) (lift . insertRepos con)
