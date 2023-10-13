module Diary.Record (
  Diary(..),
  Record(..),
  Application(..),
  Property(..),
  Contact(..)
  ) where

data Diary = Diary [Record]
           | Failed { error :: String }
           deriving (Eq, Show)

data Record = AppRecord Application
            | ContactRecord Contact
            deriving (Eq, Show)

data Application = Application { state :: String
                               , appDay :: String
                               , company :: String
                               , position :: String }
                   deriving (Eq, Show)

data Property = Property { key :: String, value :: String }
  deriving (Eq, Show)

data Contact = Contact { callDay :: String
                       , name :: String
                       , props :: [Property] }
             deriving (Eq, Show)
