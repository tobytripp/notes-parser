import Test.HUnit
import Text.Parsec
import Text.Parsec.String (Parser)
import System.Exit (exitSuccess, exitFailure)

import qualified Diary.ParserSpec as Parser
import qualified Diary.FormatSpec as Format

allTests = TestList [
  Parser.tests
  , Format.tests
  ]

main :: IO ()
main = do
  counts <- runTestTT allTests
  case ((failures counts), (errors counts)) of
    (0, 0) -> exitSuccess
    otherwise -> exitFailure
