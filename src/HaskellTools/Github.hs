module HaskellTools.Github
    ( haskellRepos
    ) where

import Github.Search ( searchRepos
                     , Error
                     , SearchReposResult
                     )

haskellRepos :: IO (Either Error SearchReposResult)
haskellRepos = searchRepos "q=a in%3Aname language%3Ahaskell created%3A>2013-10-01&per_page=100"
