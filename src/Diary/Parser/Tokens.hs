module Diary.Parser.Tokens where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (LanguageDef, emptyDef)

lexer = Token.makeTokenParser diaryDef
whitespace = Token.whiteSpace lexer
lexeme = Token.lexeme lexer

word :: Parser String
word = Token.lexeme lexer $ many1 letter

line :: Parser String
line = many1 (noneOf "\n\r")

blanklines :: Parser String
blanklines = do {whitespace; return ""}

bullet :: String -> Parser String
bullet stars = Token.symbol lexer stars <?> stars

keyword :: String -> Parser String
keyword kw = Token.symbol lexer kw

diaryDef :: LanguageDef st
diaryDef = emptyDef
           { Token.commentLine = "#"
           , Token.caseSensitive = True}
