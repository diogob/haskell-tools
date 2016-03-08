module HaskellTools.Github
    ( Repo (..)
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
import Network.Wreq.Lens (responseBody)
import Control.Lens ((^..))

import Data.Aeson.Types (Value ( String, Number ))

import qualified Data.ByteString.Lazy as BL
import Data.Aeson.Lens (key, values)

import Control.Monad (unless)
import Pipes

import Data.Text (Text)
import Data.Scientific (Scientific)

data Repo = Repo
  { name :: Text
  , owner :: Text
  , url :: Text
  , watchers :: Scientific
  , forks :: Scientific
  } deriving (Show, Eq)

baseUrl :: String
baseUrl = "https://api.github.com/"

getJSON :: Options -> String -> IO (Response BL.ByteString)
getJSON requestOptions path =
  getWith requestOptions (baseUrl ++ path)

searchRepos :: Options -> String -> IO (Response BL.ByteString)
searchRepos options query =
  getJSON options $ "search/repositories?q=" ++ query

haskellRepos :: Int -> Producer [Repo] IO ()
haskellRepos page = do
  repos <- lift $ haskellReposPage page
  unless (null repos) $ do
    yield repos
    haskellRepos $ page + 1

haskellReposPage :: Int -> IO [Repo]
haskellReposPage page = do
  results <- searchRepos defaults searchUrl
  return $ resultsToRepos $ results ^.. repos . (key "name" <> _owner <> key "html_url" <> key "watchers_count" <> key "forks_count")
  where
    _owner = key "owner" . key "login"
    repos = responseBody . key "items" . values
    searchUrl = "a in%3Aname language%3Ahaskell created%3A>2013-10-01&per_page=100&page=" <> show page
    resultsToRepos = mapMaybe listToRepo . chunksOf 5
    listToRepo [String n, String o, String u, Number w, Number f] = Just Repo
      { name = n
      , owner = o
      , url = u
      , watchers = w
      , forks = f
      }
    listToRepo _ = Nothing
