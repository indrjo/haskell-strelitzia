
module Parsers ( listPackageNames
               , listNotFounds
               , repoUrl
               , listImports
               ) where

import Control.Monad
import Text.Parsec
import Text.Parsec.String

-- DESCRIPTION
--
-- This module defines some functions that take a string as argument (the
-- contents of the standard output or some log) and to extract some names
-- with aid of parsers.
--

-- ------------------------------------------------------------------------
-- Parse the output of `tlmgr search --global --file PATTERN`.
-- ------------------------------------------------------------------------

listPackageNames :: String -> [String]
listPackageNames = listCaptures packageNameParser (:)

-- From "\nNAME:\n" extract "NAME".
packageNameParser ::  Parser String
packageNameParser =
  between newline
          (char ':' >> newline)
          (many1 $ noneOf ":")


-- ------------------------------------------------------------------------
-- Extract the names of the missing file from the log of the compilation.
-- ------------------------------------------------------------------------

-- So far the parser looks for pieces having the following pattern
--
--   <quote-mark>FILE<quote-mark> not found
--
-- and isolates just FILE. See `quoteMarks` for a list of quotation marks
-- used to wrap the name of the missing.
listNotFounds :: String -> [String]
listNotFounds = listCaptures notFoundParser (:)

-- From a string complaining of something not found of the fashion shown
-- above isolate the name.
notFoundParser :: Parser String
notFoundParser =
  between (oneOf quoteMarks)
          (oneOf quoteMarks >> spaces >> string "not found")
          (many1 $ noneOf quoteMarks)

quoteMarks :: String
quoteMarks = "'\"`"


-- ------------------------------------------------------------------------
-- Parse `tlmgr option repository` output to extract the url.
-- ------------------------------------------------------------------------

repoUrl :: String -> Either ParseError String
repoUrl = parse repoUrlParser ""

repoUrlParser :: Parser String
repoUrlParser = do
  _ <- string "Default package repository (repository):"
  spaces
  many1 $ noneOf "\n"


-- ------------------------------------------------------------------------
-- Listing required packages
-- ------------------------------------------------------------------------

listImports :: String -> [String]
listImports = listCaptures importsParser (++)

importsParser :: Parser [String]
importsParser = do
  _ <- importKeyWordParser
  spaces
  optional squareParser
  spaces
  between (char '{') (char '}') commasParser

importKeyWords :: [String]
importKeyWords =
  [ "\\documentclass"
  , "\\usepackage"
  , "\\RequirePackage"
  ]

importKeyWordParser :: Parser String
importKeyWordParser = choiceTry $ map string importKeyWords

squareParser :: Parser String
squareParser = between (char '[') (char ']') (many $ noneOf "]")

singleWordParser :: Parser String
singleWordParser = between spaces spaces (many1 alphaNum)

commasParser :: Parser [String]
commasParser = singleWordParser `sepEndBy` char ','


-- ------------------------------------------------------------------------
-- Helper functions.
-- ------------------------------------------------------------------------

listCaptures :: (Monoid b) => Parser a -> (a -> b -> b) -> String -> b
listCaptures p conc str =
  either (\_ -> mempty) id $ parse (listCapturesParser p conc) "" str

-- While consuming an input string, try a given parser: if it succeeds,
-- collect the match into a list, otherwise move on by one character. The
-- second argument of the function here is a binary function used to chain
-- in some manner the results of the applied parser.
listCapturesParser :: (Monoid b) => Parser a -> (a -> b -> b) -> Parser b
listCapturesParser p conc =
  choiceTry
    [ do x  <- p
         xs <- listCapturesParser p conc
         return $ conc x xs
    , anyChar >> listCapturesParser p conc
    , return mempty
    ]

choiceTry :: [ParsecT s u m a] -> ParsecT s u m a
choiceTry = foldr (\p q -> try p <|> q) mzero

