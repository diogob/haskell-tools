module HaskellTools.Database
    ( insertRepos
    ) where

import qualified Spyglass as S
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
insertRepo r = S.showt $ S.insert "public.repos" columns values
  where
    columns = S.fromList ["name", "owner", "url", "watchers", "forks"]
    values = S.fromList [name r, owner r, url r, s2t $ watchers r, s2t $ forks r]
    s2t = T.pack . show . fromMaybe (0 :: Int) . toBoundedInteger
