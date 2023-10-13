module Diary.Parser (
  diary, application, contact, properties
  ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Text (pack, unpack, strip)

import qualified Diary.Parser.Tokens as Tokens
import Diary.Record

diary :: Parser Diary
diary = Diary <$> manyTill record eof

record :: Parser Record
record = try (AppRecord <$> application)
  <|> ContactRecord <$> contact

date :: Parser String
date = do
  _ <- char '<' <?> "open bracket for date"
  content <- many (noneOf ">")
  _ <- char '>' <?> "end bracket for date"
  _ <- Tokens.whitespace
  return content

companyName :: Parser String
companyName = do
  _ <- Tokens.whitespace
  content <- many (noneOf ":-")
  return $ unpack (strip (pack content))

title :: Parser String
title = do
  _ <- many (oneOf ": ")
  content <- many (noneOf "\n")
  _ <- Tokens.whitespace
  return content

contactName :: Parser String
contactName = do
  _ <- Tokens.whitespace
  content <- Tokens.line
  _ <- Tokens.whitespace
  return content

propName :: Parser String
propName = do
  _ <- oneOf ":"
  content <- Tokens.word
  _ <- oneOf ":"
  _ <- many (oneOf " \t")
  return content

property :: Parser Property
property = Property
  <$> propName
  <*> (Tokens.line <* Tokens.whitespace)

properties :: Parser [Property]
properties = do
  _ <- Tokens.keyword ":PROPERTIES:"
  ps <- many (try property)
  _ <- Tokens.keyword ":END:"
  return ps

application :: Parser Application
application = Application
  <$> (noteLines *> Tokens.bullet "*" *> Tokens.word)
  <*> date
  <*> companyName
  <*> (title <* noteLines)
  <?> "application"

noteLine :: Parser String
noteLine = do
  fc <- noneOf "*"
  rest <- many (noneOf "\n\r")
  _ <- Tokens.whitespace
  return (fc:rest)

noteLines :: Parser [String]
noteLines = many noteLine

contact :: Parser Contact
contact = Contact
  <$> (Tokens.bullet "**" *> date)
  <*> contactName
  <*> (option [] properties <* noteLines)
