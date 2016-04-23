module Main where

import HaskellTools
import HaskellTools.Config
import HaskellTools.Hackage
import HaskellTools.Database

import Data.List (intercalate)
import Pipes
import Hasql.Connection
import Data.String.Conversions (cs)

main :: IO ()
main = do
  conf <- readOptions
  let dbConfig = cs $ configDatabase conf
  conOrError <- acquire dbConfig
  case conOrError of
    Left _ -> error "Error connecting"
    Right c -> runEffect $ loop c

loop :: Connection -> Effect IO ()
loop con = for (producePackages 0) (lift . insertPkgs con)
