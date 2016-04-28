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
    Right c -> do
      runEffect $ loopPackages c
      runEffect $ loopDeps c
      runEffect $ loopRepos c

loopPackages :: Connection -> Effect IO ()
loopPackages con = for (producePackages 0) (lift . insertPkgs con)

loopDeps :: Connection -> Effect IO ()
loopDeps con = for (producePackagesWithDeps 0) (lift . insertDeps con)

loopRepos :: Connection -> Effect IO ()
loopRepos con = do
  reposOrError <- lift $ fetchPackageRepos con
  case reposOrError of
    Left _ -> error "Error fetching repos"
    Right r -> for (produceRepos r 0) (lift . insertRepos con)
