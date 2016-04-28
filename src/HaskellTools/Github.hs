module HaskellTools.Github
    ( Repo (..)
    , produceRepos
    ) where

import Data.Monoid ((<>))

import Network.Wreq ( getWith
                    , Options
                    , Response
                    , defaults
                    )
import Network.Wreq.Lens (responseBody, responseStatus)
import Control.Lens ((^..), (^.))
import Control.Concurrent (threadDelay)
import Network.HTTP.Types

import qualified Data.ByteString.Lazy as BL
import Data.Aeson.Lens (key, values, _Integral)

import Control.Monad (unless)
import Pipes

import Data.Text (Text)
import qualified Data.Text as T

import HaskellTools.Types
import qualified Data.Vector as V

produceRepos :: PackageRepos -> Int -> Producer [Repo] IO ()
produceRepos pkgs page = do
  repos <- lift ioRepos
  unless (V.null repos) $ do
    lift $ threadDelay 1000 -- github api rate limit
    yield $ V.toList repos
    produceRepos pkgs $ page + 1
  where
    ioRepos = V.mapM (\(pn, o, r) -> mkRepo defaults pn o r) batch
    batch = pageSlice pkgs
    pageSlice = V.slice (page * pageSize) (page * pageSize + pageSize)
    pageSize = 1000

baseUrl :: String
baseUrl = "https://api.github.com/"

getJSON :: Options -> String -> IO (Response BL.ByteString)
getJSON requestOptions path =
  getWith requestOptions (baseUrl ++ path)

repo :: Options -> String -> String -> IO (Response BL.ByteString)
repo options o r =
  getJSON options $ "repos/" ++ o ++ "/" ++ r

repoStats :: Options -> String -> String -> IO (Response BL.ByteString)
repoStats options repoOwner repoName = do
  r <- getJSON options $ "repos/" ++ repoOwner ++ "/" ++ repoName ++ "/stats/contributors"
  if r ^. responseStatus == status202
    then threadDelay 1000 >> repoStats options repoOwner repoName
    else return r

mkRepo :: Options -> Text -> Text -> Text -> IO Repo
mkRepo options pkgName repoOwner repoName = do
  info <- repo options ro rn
  stats <- repoStats options ro rn
  let
    collab = fromIntegral $ length $ stats ^.. responseBody . values . key "author" . key "login"
    [st, fk] = info ^.. responseBody . (key "stargazers_count" <> key "forks") . _Integral
  return $ Repo pkgName st fk collab
  where
    ro = T.unpack repoOwner
    rn = T.unpack repoName
