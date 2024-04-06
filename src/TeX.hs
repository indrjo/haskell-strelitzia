
module TeX (makeTeX) where

import RunShell (simpleCallCommand)

-- Generate an appropriate shell command. "max_print_line=1000" is to avoid
-- pesky breaks in the log that circumvent the parser `notFoundsParser`.
texCommand :: FilePath -> FilePath -> String
texCommand engine texf = unwords ["max_print_line=1000", engine, texf]

-- The shell command that prints a certain amount of consecutive newlines.
-- The output is piped to another command.
shNewLines :: Int -> String
shNewLines n = "for i in $(seq 1 " ++ show n ++ "); do echo; done"

-- Pass a file to a TeX engine to produce a document. The last arguments is
-- the 'tolerance', that is the number of times the program is able to face
-- halts during the compilation. If this threshold is exceeded, user has to
-- take action.
makeTeX :: FilePath -> FilePath -> Int -> IO ()
makeTeX engine texf tol =
  simpleCallCommand $ unwords [shNewLines tol, "|", texCommand engine texf]

