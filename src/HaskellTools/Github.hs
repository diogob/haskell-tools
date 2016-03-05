module HaskellTools.Github
    ( searchRepos
    , haskellRepos
    ) where

import Data.Monoid ((<>))

import Network.Wreq ( getWith
                    , Options
                    , Response
                    , asJSON
                    , header
                    , defaults
                    )
import Network.Wreq.Lens (responseBody)
import Control.Lens ( (&)
                    , (.~)
                    , (^..)
                    )

import Data.Aeson.Types (FromJSON
                        , Object
                        , Value
                        )

import qualified Data.ByteString.Lazy as BL
import Data.Aeson.Lens (key, _String, values)

import Control.Monad (unless)
import Pipes

baseUrl :: String
baseUrl = "https://api.github.com/"

getJSON :: Options -> String -> IO (Response BL.ByteString)
getJSON requestOptions path =
  getWith requestOptions (baseUrl ++ path)

searchRepos :: Options -> String -> IO (Response BL.ByteString)
searchRepos options query =
  getJSON options $ "search/repositories?q=" ++ query

haskellRepos :: Int -> Producer [Value] IO ()
haskellRepos page = do
  repos <- lift $ haskellReposPage page
  unless (null repos) $ do
    yield repos
    haskellRepos $ page + 1

haskellReposPage :: Int -> IO [Value]
haskellReposPage page = do
  results <- searchRepos defaults url
  return $ results ^.. repos . (key "name" <> key "watchers" <> key "forks_count" <> owner)
  where
    owner = key "owner" . key "login"
    repos = responseBody . key "items" . values
    url = "a in%3Aname language%3Ahaskell created%3A>2013-10-01&per_page=100&page=" <> show page
