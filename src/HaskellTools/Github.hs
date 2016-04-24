module HaskellTools.Github
    ( Repo (..)
    , mkRepo
    , haskellRepos
    ) where

import Data.Monoid ((<>))

import Data.List.Split (chunksOf)

import Data.Maybe (mapMaybe)

import Network.Wreq ( getWith
                    , Options
                    , Response
                    , defaults
                    )
import Network.Wreq.Lens (responseBody, responseStatus)
import Control.Lens ((^..), (^.))
import Control.Concurrent (threadDelay)
import Network.HTTP.Types

import Data.Aeson.Types (Value ( String, Number ))

import qualified Data.ByteString.Lazy as BL
import Data.Aeson.Lens (key, values, _String, _Integral)

import Control.Monad (unless)
import Pipes

import Data.Text (Text)
import Data.Scientific (Scientific)

data Repo = Repo
  { stars :: Integer
  , forks :: Integer
  , collaborators :: Integer
  } deriving (Show, Eq)

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

mkRepo :: Options -> String -> String -> IO Repo
mkRepo options repoOwner repoName = do
  info <- repo options repoOwner repoName
  stats <- repoStats options repoOwner repoName
  let
    collab = fromIntegral $ length $ stats ^.. responseBody . values . key "author" . key "login"
    [st, fk] = info ^.. responseBody . (key "stargazers_count" <> key "forks") . _Integral
  return $ Repo st fk collab

haskellRepos :: Int -> Producer [Repo] IO ()
haskellRepos page = do
  repos <- lift $ undefined
  unless (null repos) $ do
    yield repos
    haskellRepos $ page + 1
