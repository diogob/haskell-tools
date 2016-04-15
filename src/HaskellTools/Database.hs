module HaskellTools.Database
    ( insertRepos
    , insertPkgs
    ) where

import Pasta ( insert
             , doUpdate
             , onConflict
             , (=.=)
             , (//)
             , showt
             , fromList
             )
import qualified Hasql.Connection as H
import qualified Hasql.Session as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Monoid ((<>))
import HaskellTools.Github
import Data.Scientific (toBoundedInteger)
import Data.Maybe (fromMaybe)
import Control.Lens
import Distribution.PackageDescription
import Distribution.Package
import Distribution.Text

insertRepos :: H.Connection -> [Repo] -> IO ()
insertRepos con repos =
  runSQL $ T.encodeUtf8 cmds
  where
    cmds = T.concat $ insertRepo <$> repos
    runSQL cmd = do
      resOrError <- H.run (H.sql cmd) con
      case resOrError of
        Left _ -> print ("Error inserting: " <> cmds :: T.Text)
        Right _ -> print ("Inserted repo info" :: String)

insertRepo :: Repo -> T.Text
insertRepo r =
  showt $
  insert "public.repos" columns values
  & onConflict .~ doUpdate "repos_pkey" ["watchers" =.= ("EXCLUDED"//"watchers"), "forks" =.= ("EXCLUDED"//"forks")]
  where
    columns = fromList ["name", "owner", "url", "watchers", "forks"]
    values = fromList [name r, owner r, url r, s2t $ watchers r, s2t $ forks r]
    s2t = T.pack . show . fromMaybe (0 :: Int) . toBoundedInteger

insertPkgs :: H.Connection -> [PackageDescription] -> IO ()
insertPkgs con pkgs =
  runSQL $ T.encodeUtf8 cmds
  where
    cmds = T.concat $ insertPkg <$> pkgs
    runSQL cmd = do
      resOrError <- H.run (H.sql cmd) con
      case resOrError of
        Left _ -> print ("Error inserting: " <> cmds :: T.Text)
        Right _ -> print ("Inserted repo info" :: String)

insertPkg :: PackageDescription -> T.Text
insertPkg p =
  showt $
  insert "public.packages" columns values
  & onConflict .~ doUpdate "packages_pkey" ["version" =.= ("EXCLUDED"//"version")]
  where
    columns = fromList ["name", "version", "license", "description", "category", "homepage", "package_url", "repo_type", "repo_location"]
    values = fromList [p2name p, p2version p, p2license p, p2desc p, p2cat p, p2homepage p, p2pkgUrl p, p2repoType, p2repoLocation]
    p2name = T.pack . display . pkgName . package
    p2version = T.pack . display . pkgVersion . package
    p2license = T.pack . display . license
    p2desc = T.pack . description
    p2cat = T.pack . category
    p2homepage = T.pack . homepage
    p2pkgUrl = T.pack . pkgUrl
    repos = sourceRepos p
    reposTypes = map (T.pack . display . fromMaybe (OtherRepoType "Unkonwn") . repoType) repos
    reposLocations = map (T.pack . fromMaybe "NULL" . repoLocation) repos
    p2repoType = if null reposTypes
                    then "NULL"
                    else head reposTypes
    p2repoLocation = if null reposLocations
                        then "NULL"
                        else head reposLocations
