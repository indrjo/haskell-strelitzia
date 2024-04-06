
module Say ( say
           , sayError
           , sayErrorAndDie
           ) where

import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

preSay :: String
preSay = "::"

say :: String -> IO ()
say msg = putStrLn $ unwords [preSay, msg]

preSayError :: String
preSayError = "!!!"

sayError :: String -> IO ()
sayError msg = hPutStrLn stderr $ unwords [preSayError, msg]

sayErrorAndDie :: String -> IO ()
sayErrorAndDie msg = sayError msg >> exitFailure

