module Parakeet.Types.Token (
  Token (..)
) where

import qualified Parakeet.Types.Lexeme as L

data Token a = Line
             | Lit L.Lit
             | AlphaNum L.AlphaNum (Maybe ([L.Hiragana], [L.Katakana], [L.Romaji a]))
             | Kanji L.Kanji [L.Hiragana] [L.Katakana] [L.Romaji a]
             | Hiragana L.Hiragana [L.Romaji a]
             | Katakana L.Katakana [L.Romaji a]
             deriving (Show)
