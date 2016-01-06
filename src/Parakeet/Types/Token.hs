module Parakeet.Types.Token (
  Token(..)
) where

import qualified Parakeet.Types.Lexeme as Lexeme

data Token a = Line
             | Break
             | Lit Lexeme.Lit
             | Kanji Lexeme.Kanji [Lexeme.Hiragana] [Lexeme.Katakana] [Lexeme.Romaji a]
             | Hiragana Lexeme.Hiragana [Lexeme.Romaji a]
             | Katakana Lexeme.Katakana [Lexeme.Romaji a]
             deriving (Show)
