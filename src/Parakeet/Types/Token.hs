module Parakeet.Types.Token (
  Token(..)
) where

import qualified Parakeet.Types.Lexeme as Lexeme

data Token
    = Line
    | Break
    | Lit Lexeme.Lit
    | Kanji Lexeme.Kanji [Lexeme.Hiragana] [Lexeme.Katakana] [Lexeme.Romaji]
    | Hiragana Lexeme.Hiragana [Lexeme.Romaji]
    | Katakana Lexeme.Katakana [Lexeme.Romaji]
    deriving (Show)

