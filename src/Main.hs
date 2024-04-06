{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Conditional (when, unlessM)
import System.FilePath (replaceExtension)
import System.Directory (doesFileExist)

import Parsers (listNotFounds, listImports)
import TeX (makeTeX)
import Tlmgr (tlmgrSearch, tlmgrInstall, contactPackageRepo)
import CLI (KeyVals(..), getMainCLIArgs)
import Say (say, sayError, sayErrorAndDie)


main :: IO ()
main = do
  
  -- Parse the CLI options here: key-val's and what remains of `getArgs`.
  -- `remArgs` here is the list of commands that where not crunched during
  -- the parse of the command line parameters.
  (KeyVals engine m_importsFile maxHalts, remArgs) <- getMainCLIArgs

  -- Make sure the head of `remArgs` is the main file of your TeX project!
  -- The tail is ignored.
  when (null remArgs) $
    sayErrorAndDie "no main file specified!"
    
  let mainFile = head remArgs in do
    
    unlessM (doesFileExist mainFile) $
      sayErrorAndDie $ mainFile ++ " does not exist!"

    -- If the file of the import is provided, use it to extract the
    -- names of the packages imported there and try to install them.
    case m_importsFile of
      Just importsFile -> do
        unlessM (doesFileExist importsFile) $
          sayErrorAndDie $ importsFile ++ " does not exist!"
        imports <- listImports <$> readFile importsFile
        say $ "packages needed: " ++ unwords imports
        tlmgrInstall imports
      _ -> return ()

    strelitzia engine mainFile maxHalts


strelitzia :: FilePath -> FilePath -> Int -> IO ()
strelitzia engine mainFile maxHalts = do
  makeTeX engine mainFile maxHalts
  notFounds <- listNotFounds <$> readFile (replaceExtension mainFile "log")
  if null notFounds
    then
      say "no missing file! good..."
    else do
      sayError $ "missing files: " ++ unwords notFounds
      connected <- contactPackageRepo
      if connected
        then do
          packages <- tlmgrSearch notFounds
          when (null packages) $
            sayErrorAndDie "no packages to install!"
          say $ "packages to be installed: " ++ unwords packages
          tlmgrInstall packages
        else do
          sayErrorAndDie "cannot reach the package repository!"

