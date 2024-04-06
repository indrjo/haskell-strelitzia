
module CLI (KeyVals(..), getMainCLIArgs) where

import Options

data KeyVals =
  KeyVals FilePath         -- the TeX engine
          (Maybe FilePath) -- the file of the imports
          Int              -- max number of halts handled by strelitzia

instance Options KeyVals where
  defineOptions =
    pure KeyVals
      <*> simpleOption "engine"
                       "pdflatex"
                       "indicate the TeX engine to use"
      <*> simpleOption "check-imports"
                       Nothing
                       "the file of the imports"
      <*> simpleOption "max-halts"
                       50
                       "maximum amount of halts handled by the program"

getMainCLIArgs :: IO (KeyVals, [String])
getMainCLIArgs = runCommand $ \kvs remArgs -> return $ (kvs, remArgs)

