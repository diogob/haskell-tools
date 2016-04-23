module HaskellTools.Hackage ( producePackages
                            , producePackagesWithDeps
                            , PackageWithDeps
                            ) where

import qualified Distribution.Hackage.DB as DB
import Distribution.Package
import Distribution.PackageDescription
import Language.Haskell.Extension

import Control.Monad (unless, liftM)
import Pipes
import Data.Maybe

type PackageWithDeps = (PackageDescription, [Extension], [Dependency])

producePackages :: Int -> Producer [PackageDescription] IO ()
producePackages page = do
  pkgs <- lift $ packages page
  unless (null pkgs) $ do
    yield pkgs
    producePackages $ page + 1

producePackagesWithDeps :: Int -> Producer [PackageWithDeps] IO ()
producePackagesWithDeps page = do
  pkgs <- lift $ packagesWithDeps page
  unless (null pkgs) $ do
    yield pkgs
    producePackagesWithDeps $ page + 1

packages :: Int -> IO [PackageDescription]
packages page = liftM (map (\(p, _, _) -> p)) $ packagesWithDeps page

packagesWithDeps :: Int -> IO [PackageWithDeps]
packagesWithDeps page = liftM (pageSlice . pkgsWithDeps . DB.toAscList) DB.readHackage
    where
      latestVersion = head . DB.toDescList . snd
      extractBuildInfo gp = (packageDescription gp, extractExts gp, extractDeps gp)
      extractExts gp = lib2exts (condLibrary gp) ++ exe2exts (condExecutables gp)
      extractDeps gp = lib2deps (condLibrary gp) ++ exe2deps (condExecutables gp)
      exe2deps = concatMap (condTreeConstraints . snd)
      lib2deps = concatMap condTreeConstraints . maybeToList
      lib2exts = buildInfo2exts . lib2buildInfo
      exe2exts = buildInfo2exts . exe2buildInfo
      buildInfo2exts = concatMap (\b -> defaultExtensions b
                                        ++ otherExtensions b
                                        ++ oldExtensions b)
      exe2buildInfo = map (buildInfo . condTreeData . snd)
      lib2buildInfo = map (libBuildInfo . condTreeData) . maybeToList
      pkgsWithDeps = map (extractBuildInfo . snd . latestVersion)
      slice from to xs = take (to - from + 1) (drop from xs)
      pageSlice = slice (page * pageSize) (page * pageSize + pageSize)
      pageSize = 1000
