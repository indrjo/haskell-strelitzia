
module Tlmgr ( tlmgrInstall
             , tlmgrSearch
             , contactPackageRepo
             ) where

import Data.List (intercalate)
import RunShell (runShell, simpleCallCommand, wgetSpider)
import Parsers (listPackageNames, repoUrl)


-- DESCRIPTION
--
-- A minimal TeX Live has *tlmgr* to handle packages: not only you install
-- packages with it, but you can search packages containing a given file!
--

-- ------------------------------------------------------------------------
-- Search for packages.
-- ------------------------------------------------------------------------

-- Take a list of files not found and list the packages containing them.
tlmgrSearch :: [String] -> IO [String]
tlmgrSearch notFounds =
  listPackageNames <$> runShell ("tlmgr search --global --file "
                                 ++ searchPattern notFounds) ""

searchPattern :: [String] -> String
searchPattern []     = ""
searchPattern [name] = "'/" ++ name ++ "'"
searchPattern names  = "'/(" ++ intercalate "|" names ++ ")'"


-- ------------------------------------------------------------------------
-- Install packages.
-- ------------------------------------------------------------------------

tlmgrInstall :: [String] -> IO ()
tlmgrInstall packages =
  simpleCallCommand $ "tlmgr install " ++ unwords packages


-- ------------------------------------------------------------------------
-- Other.
-- ------------------------------------------------------------------------

-- Check if you can contact the default repository.
contactPackageRepo :: IO Bool
contactPackageRepo = do
  proc_out <- runShell "tlmgr option repository" ""
  case repoUrl proc_out of
    Left err -> do
      -- Just in case... who knows?
      putStrLn "contactPackageRepo: error during parsing"
      putStrLn $ show err
      return False
    Right url -> do
      putStrLn $ "trying to contact " ++ url
      wgetSpider url

