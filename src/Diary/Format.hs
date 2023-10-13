module Diary.Format (format) where

import Data.Text (toLower, pack, unpack)

import Diary.Record

class Formatable a where
  format :: a -> String

instance Formatable Diary where
  format (Diary rs) = unlines $ map format rs
  format (Failed e) = e

instance Formatable Record where
  format r =
    case r of
      AppRecord a -> unlines [
        (company a)
        , "\t" ++ (state a)
        , "\tDate of Application/First Contact: " ++ (appDay a)
        , "\tPosition: " ++ (position a)
        ]
      ContactRecord c -> unlines ([
        "\tDate of Contact: " ++ (callDay c)
        , "\tContact Person: " ++ (name c)
        ] ++ map format (props c))

instance Formatable Property where
  format p = "\t" ++ (lowerKey p) ++ ": " ++ (value p)
    where
      lowerKey = unpack . toLower . pack . key
