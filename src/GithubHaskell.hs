module GithubHaskell
    ( haskellRepos
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
                    , (^.)
                    )
import Data.Aeson.Types (FromJSON, Object)

baseUrl :: String
baseUrl = "https://api.github.com/"

getJSON :: FromJSON a => Options -> String -> IO (Response a)
getJSON requestOptions path =
    asJSON =<< getWith requestOptions (baseUrl ++ path)

getOne :: FromJSON a => Options -> String -> IO a
getOne requestOptions path = do
  r <- getJSON requestOptions path
  return $ r ^. responseBody

searchRepos :: Options -> String -> IO Object
searchRepos options query = 
    getOne options $ "search/repositories?q=" ++ show query

haskellRepos :: Int -> IO Object
haskellRepos page = searchRepos defaults $ "a in%3Aname language%3Ahaskell created%3A>2013-10-01&per_page=100&page=" <> show page
