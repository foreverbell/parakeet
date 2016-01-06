module Parakeet.Types.FToken (
  FToken(..)
) where

data FToken = Line
            | Break
            | Lit String
            | Kanji String [String] [String]
            | Hiragana String [String]
            | Katakana String [String]
            deriving (Show)
