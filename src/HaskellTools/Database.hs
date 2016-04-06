module HaskellTools.Database
    ( insertRepos
    ) where

import qualified Pasta as P
import qualified Hasql.Connection as H
import qualified Hasql.Session as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Monoid ((<>))
import HaskellTools.Github
import Data.Scientific (toBoundedInteger)
import Data.Maybe (fromMaybe)

insertRepos :: H.Connection -> [Repo] -> IO ()
insertRepos con repos =
  runSQL $ T.encodeUtf8 cmds
  where
    cmds = T.intercalate ";" $ insertRepo <$> repos
    runSQL cmd = do
      resOrError <- H.run (H.sql cmd) con
      case resOrError of
        Left _ -> print ("Error inserting: " <> cmds :: T.Text)
        Right _ -> print ("Inserted repo info" :: String)

insertRepo :: Repo -> T.Text
insertRepo r = P.showt $ P.insert "public.repos" columns values
  where
    columns = P.fromList ["name", "owner", "url", "watchers", "forks"]
    values = P.fromList [name r, owner r, url r, s2t $ watchers r, s2t $ forks r]
    s2t = T.pack . show . fromMaybe (0 :: Int) . toBoundedInteger
