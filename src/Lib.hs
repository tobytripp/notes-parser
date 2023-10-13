module Lib
    ( parseDiary
    ) where

import Text.Parsec (runParser)
import Text.Printf

import Diary.Record (Diary(..))
import Diary.Parser

parseDiary :: FilePath -> IO Diary
parseDiary path = do
  input <- readFile path
  case runParser diary () path input of
    (Left e) -> return $ Failed (printf "Parse error: '%s'" (show e))
    (Right d) -> return $ d
