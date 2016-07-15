module HaskellTools.Database
    ( insertPkgs
    , insertDeps
    , insertRepos
    , fetchPackageRepos
    ) where

import Pasta ( selectFrom
             , conditions
             , insert
             , doUpdate
             , doNothing
             , onConflict
             , (.|)
             , (.=)
             , (//)
             , toSQL
             , fromList
             , BooleanExpression (..)
             , columns
             , update
             , returning
             , now
             , age
             , gte
             )

import qualified Hasql.Connection as H
import qualified Hasql.Session as H
import qualified Hasql.Query as H
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Monoid ((<>))
import HaskellTools.Hackage
import HaskellTools.Types
import Data.Maybe (fromMaybe)
import Control.Lens hiding ((.=))
import Distribution.PackageDescription
import Distribution.Package
import Distribution.Text
import Language.Haskell.Extension
import Data.String.Conversions


insertRepos :: H.Connection -> [Repo] -> IO ()
insertRepos = runInserts insertRepo

insertPkgs :: H.Connection -> [PackageDescription] -> IO ()
insertPkgs = runInserts insertPkg

insertDeps :: H.Connection -> [PackageWithDeps] -> IO ()
insertDeps = runInserts insertPackageWithDeps

showt :: Show a => a -> T.Text
showt = cs . show

insertRepo :: Repo -> T.Text
insertRepo r =
  toSQL $
  insert "public.repos" cols values
  & onConflict .~ doUpdate "repos_pkey" ["stars" .= ("EXCLUDED"//"stars"), "forks" .= ("EXCLUDED"//"forks"), "collaborators" .= ("EXCLUDED"//"collaborators")]
  where
    cols = fromList ["package_name", "stars", "forks", "collaborators"]
    values = fromList [repoPackageName r, showt $ stars r, showt $ forks r, showt $ collaborators r]

runInserts :: (a -> T.Text) -> H.Connection -> [a] -> IO ()
runInserts insertFn con pkgs =
  mapM_ (runSQL . T.encodeUtf8) cmds
  where
    cmds = insertFn <$> pkgs
    runSQL cmd = do
      resOrError <- H.run (H.sql cmd) con
      case resOrError of
        Left _ -> print ("Error inserting..." :: String)
        Right _ -> print ("Inserted..." :: String)

p2name :: PackageDescription -> T.Text
p2name = T.pack . display . pkgName . package

insertPkg :: PackageDescription -> T.Text
insertPkg p =
  toSQL $
  insert "public.packages" cols values
  & onConflict .~ doUpdate "packages_pkey" ["version" .= ("EXCLUDED"//"version")]
  where
    cols = fromList ["package_name", "version", "license", "description", "category", "homepage", "package_url", "repo_type", "repo_location"]
    values = fromList [p2name p, p2version p, p2license p, p2desc p, p2cat p, p2homepage p, p2pkgUrl p, p2repoType, p2repoLocation]
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

insertPackageWithDeps :: PackageWithDeps -> T.Text
insertPackageWithDeps (pd, e, d) =
  T.concat (insertDep pd <$> d)
  <> T.concat (insertExt pd <$> e)

insertDep :: PackageDescription -> Dependency -> T.Text
insertDep p d =
  toSQL $
  insert "public.dependencies" cols values
  & onConflict .~ doUpdate "dependencies_pkey" ["version_range" .= ("EXCLUDED"//"version_range")]
  where
    cols = fromList ["dependent", "dependency", "version_range"]
    values = fromList [p2name p, d2depdency d, d2version d]
    d2depdency (Dependency n _) = T.pack $ display n
    d2version  (Dependency _ v) = T.pack $ display v

insertExt :: PackageDescription -> Extension -> T.Text
insertExt p e =
  toSQL $
  insert "public.extensions" cols values
  & onConflict .~ doNothing
  where
    cols = fromList ["package_name", "extension"]
    values = fromList [p2name p, e2extension e]
    e2extension = T.pack . display

fetchPackageRepos :: H.Connection -> IO (Either H.Error PackageRepos)
fetchPackageRepos =
  H.run $ H.query () selectPackageRepos

decodePackageRepos :: HD.Result PackageRepos
decodePackageRepos =
  HD.rowsVector $ (,,) <$> HD.value HD.text <*> HD.value HD.text <*> HD.value HD.text

selectPackageRepos :: H.Query () PackageRepos
selectPackageRepos =
  H.statement template HE.unit decodePackageRepos False
  where
    -- @TODO: we should change this to an UPDATE so we can record the last time we tried to fetch a repo
    template =
      T.encodeUtf8 $ toSQL $ update "package_repos" (fromList ["updated_at"]) (fromList [now])
      & conditions .~ (("package_repos"//"package_name") `In` ( selectFrom "repos"
                                                                & columns .~ fromList ["package_name"]
                                                                & conditions .~ age ("repos"//"updated_at") `gte` ("1 day" :: T.Text)
                                                                )
                        .| (age ("package_repos"//"updated_at") `gte` ("2 days" :: T.Text))
                        )
      & returning .~ ["*"]
