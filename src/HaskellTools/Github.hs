module HaskellTools.Github
    ( Repo (..)
    , produceRepos
    ) where

import Data.Monoid ((<>))

import Network.Wreq ( getWith
                    , Options
                    , Response
                    , defaults
                    , basicAuth
                    , statusCode
                    )

import Network.HTTP.Client (HttpException(..))
import Data.Maybe (catMaybes)

import Network.Wreq.Lens (responseBody, responseStatus, auth)
import Control.Lens ((^..), (^.), (?~), (&))
import Control.Concurrent (threadDelay)
import Network.HTTP.Types hiding (statusCode)

import qualified Data.ByteString.Lazy as BL
import Data.Aeson.Lens (key, values, _Integral)

import Control.Monad (unless)
import Pipes

import Data.Text (Text)
import qualified Data.Text as T

import HaskellTools.Config
import HaskellTools.Types
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BS

import Control.Exception as E

produceRepos :: AppConfig -> PackageRepos -> Int -> Producer [Repo] IO ()
produceRepos conf pkgs page = do
  repos <- lift ioRepos
  unless (V.null repos) $ do
    lift $ threadDelay 1000
    yield $ catMaybes $ V.toList repos
    produceRepos conf pkgs $ page + 1
  where
    opts = defaults & auth ?~ basicAuth (BS.pack $ ghUser conf) (BS.pack $ ghPass conf)
    ioRepos = V.mapM (\(pn, o, r) -> mkRepo opts pn o r) batch
    batch = pageSlice pkgs
    pageSlice = V.slice (page * pageSize) (page * pageSize + pageSize)
    pageSize = 10

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

mkRepo :: Options -> Text -> Text -> Text -> IO (Maybe Repo)
mkRepo options pkgName repoOwner repoName =
  getRepo `E.catch` handler
  where
    handler e@(StatusCodeException s _ _)
      | s ^. statusCode == 404 = return Nothing
      | otherwise              = throwIO e
    getRepo = do
      info <- repo options ro rn
      stats <- repoStats options ro rn
      let
        collab = fromIntegral $ length $ stats ^.. responseBody . values . key "author" . key "login"
        [st, fk] = info ^.. responseBody . (key "stargazers_count" <> key "forks") . _Integral
      return $ Just $ Repo pkgName st fk collab
      where
        ro = T.unpack repoOwner
        rn = T.unpack repoName
