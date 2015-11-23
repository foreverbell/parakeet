module Parakeet.Types.FlatToken (
  FlatToken(..)
) where

data FlatToken = Line
               | Break
               | Lit String
               | Kanji String [String] [String]
               | Hiragana String [String]
               | Katakana String [String]
               deriving (Show)
