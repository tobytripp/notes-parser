module Main (main) where

import Lib
import System.Environment

import Diary.Format

main :: IO ()
main = do
  (f:_) <- getArgs
  d <- parseDiary f
  putStr $ format d
