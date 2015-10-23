module Parser.MetaInfo (
  Author(..)
, Title(..)
, MetaInfo(..)
, getTitle
, getAuthor
, getLitAuthor
) where

import Parser.Token

newtype Title = Title [Token] deriving (Show)
newtype Author = Author ([Token], [Token]) deriving (Show)
newtype MetaInfo = MetaInfo (Title, Author) deriving (Show)

getTitle :: MetaInfo -> [Token]
getTitle (MetaInfo (Title title, _)) = title

getAuthor :: MetaInfo -> [Token]
getAuthor (MetaInfo (_, Author (author, _))) = author

getLitAuthor :: MetaInfo -> [Token]
getLitAuthor (MetaInfo (_, Author (_, author))) = author

