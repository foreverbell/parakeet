module Parakeet.Types.Token (
  Token(..)
) where

import qualified Parakeet.Types.Lexeme as Lexeme
import           Parakeet.Types.Lexeme (Single)

data Token = Line
           | Break
           | Lit Lexeme.Lit
           | Kanji Lexeme.Kanji [Lexeme.Hiragana] [Lexeme.Katakana] [Lexeme.Romaji Single]
           | Hiragana Lexeme.Hiragana [Lexeme.Romaji Single]
           | Katakana Lexeme.Katakana [Lexeme.Romaji Single]
           deriving (Show)
