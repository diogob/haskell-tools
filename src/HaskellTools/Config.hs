module HaskellTools.Config ( prettyVersion
                        , minimumPgVersion
                        , readOptions
                        , AppConfig (..)
                        )
       where

import           Control.Applicative
import qualified Data.ByteString.Char8       as BS
import           Data.List                   (intercalate)
import           Data.String.Conversions     (cs)
import           Data.Text                   (strip)
import           Data.Version                (versionBranch)
import           Options.Applicative
import           Paths_haskell_tools             (version)

-- | Data type to store all command line options
data AppConfig = AppConfig {
    configDatabase  :: String
  }

argParser :: Parser AppConfig
argParser = AppConfig
  <$> argument str (help "(REQUIRED) database connection string, e.g. postgres://user:pass@host:port/db" <> metavar "DB_URL")

-- | User friendly version number
prettyVersion :: String
prettyVersion = intercalate "." $ map show $ versionBranch version

-- | Tells the minimum PostgreSQL version required by this version of Haskell Tools
minimumPgVersion :: Integer
minimumPgVersion = 90500

-- | Function to read and parse options from the command line
readOptions :: IO AppConfig
readOptions = customExecParser parserPrefs opts
  where
    opts = info (helper <*> argParser) $
                    fullDesc
                    <> progDesc (
                    "haskell-tools-load "
                    <> prettyVersion
                    <> " / create a REST API to an existing Postgres database"
                    )
    parserPrefs = prefs showHelpOnError