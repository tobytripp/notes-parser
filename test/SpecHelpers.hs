module SpecHelpers where

import Test.HUnit
import Text.Parsec
import Text.Parsec.String

assertParsesTo :: (Eq expected, Show expected)
  => Parsec SourceName () expected -- parser to test
  -> String -- input
  -> expected
  -> IO ()
assertParsesTo p input expected =
  case runParser p nullState input input of
    (Left e) -> assertFailure $ show e
    (Right actual) -> assertEqual "" expected actual
  where
    nullState = ()
