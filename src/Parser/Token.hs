module Parser.Token (
  Token(..)
) where

data Token
    = Line
    | Break
    | Lit String
    | Kanji String [String] [String]  -- kanji, kana, romaji
    | Hiragana String [String]        -- hiragana, romaji
    | Katakana String [String]        -- katakana, romaji
    deriving (Show)

