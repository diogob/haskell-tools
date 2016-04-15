module HaskellTools.Hackage where

import qualified Distribution.Hackage.DB as DB
import Distribution.PackageDescription

import Control.Monad (unless)
import Pipes

haskellPackages :: Int -> Producer [PackageDescription] IO ()
haskellPackages page = do
  pkgs <- lift $ packages page
  unless (null pkgs) $ do
    yield pkgs
    haskellPackages $ page + 1

packages :: Int -> IO [PackageDescription]
packages page = do
  db <- DB.readHackage
  return $ pageSlice $ pkgDescriptions $ DB.toAscList db
    where
      latestVersion = head . DB.toDescList . snd
      pkgDescriptions = map (packageDescription . snd . latestVersion)
      slice from to xs = take (to - from + 1) (drop from xs)
      pageSlice = slice (page * pageSize) (page * pageSize + pageSize)
      pageSize = 10
