module Diary.ParserSpec (tests) where

import Test.HUnit
import SpecHelpers

import Text.Parsec (runParser, many)
import Text.Printf (printf)

import Diary.Parser
import Diary.Record

tests = test [
  "parse a solitary application" ~: do
      let input = "* APPLIED <date> company name : position title"
          app = Application "APPLIED" "date" "company name" "position title"
      assertParsesTo application input app
  ,
  "parse a solitary contact event" ~: do
      let input = "** <date> contact name\n:PROPERTIES:\n:EMAIL:  test@example.com\n:PHONE:  312.555.0123\n:END:\n"
          c = Contact "date" "contact name" [
            Property "EMAIL" "test@example.com"
            , Property "PHONE" "312.555.0123"
            ]
      assertParsesTo contact input c
  ,
  "parse diary entry" ~: do
      let input = unlines [
            "* COMPLETE <date time> company name : position"
            , "** <date time> contact name"
            , ":PROPERTIES:"
            , ":EMAIL:  test@test.com"
            , ":END:"
            ]
      assertParsesTo diary input
        (Diary [
            AppRecord (Application "COMPLETE" "date time" "company name"
                         "position")
            , ContactRecord (Contact "date time" "contact name" [
                                Property "EMAIL" "test@test.com"
                                ])])
  ,
  "parse diary entry with no properties" ~: do
      let input = unlines [
            "* COMPLETE <date time> company name : position"
            , "** <date time> contact name"
            ]
      assertParsesTo diary input
        (Diary [
            AppRecord (Application "COMPLETE" "date time" "company name"
                         "position")
            , ContactRecord (Contact "date time" "contact name" [])])
  ,
  "parse diary entry with no contact" ~: do
      let input = unlines [
            "* COMPLETE <date time> company name : position"
            ]
      assertParsesTo diary input
        (Diary [
            AppRecord (Application "COMPLETE" "date time" "company name"
                         "position")
            ])
          ,
  "parse diary entry with notes" ~: do
      let input = unlines [
            "#+AUTHOR: Toby Tripp"
            , ""
            , "* COMPLETE <date time> company name : position"
            , "** <date time> contact name"
            , ""
            , "Notes taken during call"
            , ""
            ]
      assertParsesTo diary input
        (Diary [
            AppRecord (Application "COMPLETE" "date time" "company name"
                         "position")
            , ContactRecord (Contact "date time" "contact name" [])])
          ,
  "parse properties" ~: do
      let input = unlines [
            ":PROPERTIES:"
            , ":EMAIL:  test@test.com"
            , ":END:"
            ]
      assertParsesTo properties input [Property "EMAIL" "test@test.com"]
          ,
  "parse application entry with notes" ~: do
      let input = unlines [
            "#+AUTHOR: Toby Tripp"
            , ""
            , "* COMPLETE <date time> company name : position"
            , ""
            , "Notes"
            , "** <date time> contact name"
            , ":PROPERTIES:"
            , ":EMAIL:  test@test.com"
            , ":END:"
            , ""
            , "Notes taken during call"
            , ""
            ]
      assertParsesTo diary input
        (Diary [
            AppRecord (Application "COMPLETE" "date time" "company name"
                         "position")
            , ContactRecord (Contact "date time" "contact name" [
                                Property "EMAIL" "test@test.com"
                                ])])

  ]
