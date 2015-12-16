module HaskellTools.Hackage where

import qualified Distribution.Hackage.DB as DB

printPackages :: IO ()
printPackages = do
  db <- DB.readHackage
  print db
