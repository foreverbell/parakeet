module Parakeet.Parser.MetaInfo (
  Author(..)
, Title(..)
, MetaInfo(..)
, getTitle
, getAuthor
, getLitAuthor
) where

import Parakeet.Parser.FlatToken

newtype Title = Title [FlatToken] deriving (Show)
newtype Author = Author ([FlatToken], [FlatToken]) deriving (Show)
newtype MetaInfo = MetaInfo (Title, Author) deriving (Show)

getTitle :: MetaInfo -> [FlatToken]
getTitle (MetaInfo (Title title, _)) = title

getAuthor :: MetaInfo -> [FlatToken]
getAuthor (MetaInfo (_, Author (author, _))) = author

getLitAuthor :: MetaInfo -> [FlatToken]
getLitAuthor (MetaInfo (_, Author (_, author))) = author

