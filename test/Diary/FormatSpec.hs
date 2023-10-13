module Diary.FormatSpec (tests) where

import Test.HUnit
import SpecHelpers

import Diary.Format
import Diary.Record

tests = test [
  "format application" ~: do
      let input = (Diary [
            AppRecord (Application "COMPLETE" "date time" "company name"
                         "position")
            , ContactRecord (Contact "time" "contact name" [
                                Property "EMAIL" "test@test.com"
                                , Property "PHONE" "312.555.0123"
                                ])])
          expected = unlines [
            "company name"
            , "\tCOMPLETE"
            , "\tDate of Application/First Contact: date time"
            , "\tPosition: position"
            , ""
            , "\tDate of Contact: time"
            , "\tContact Person: contact name"
            , "\temail: test@test.com"
            , "\tphone: 312.555.0123"
            , ""
            ]
      assertEqual "Mismatched format" expected (format input)
  ]
