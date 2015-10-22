module Parser.Token (
  Token(..)
, Author(..)
, Title(..)
, MetaInfo(..)
, getTitle
, getAuthor
, getLitAuthor
) where

data Token
    = Line
    | Break
    | Lit String
    | Kanji String [String] [String]  -- kanji, kana, romaji
    | Hiragana String [String]        -- hiragana, romaji
    | Katakana String [String]        -- katakana, romaji
    deriving (Show)

newtype Title = Title [Token] deriving (Show)
newtype Author = Author ([Token], [Token]) deriving (Show)
newtype MetaInfo = MetaInfo (Title, Author) deriving (Show)

getTitle :: MetaInfo -> [Token]
getTitle (MetaInfo (Title title, _)) = title

getAuthor :: MetaInfo -> [Token]
getAuthor (MetaInfo (_, Author (author, _))) = author

getLitAuthor :: MetaInfo -> [Token]
getLitAuthor (MetaInfo (_, Author (_, author))) = author

