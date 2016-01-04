module Parakeet.Types.Meta (
  Author(..)
, Title(..)
, Meta(..)
, getTitle
, getAuthor
, getLitAuthor
) where

import Parakeet.Types.FlatToken (FlatToken)

newtype Title = Title [FlatToken] deriving (Show)
newtype Author = Author ([FlatToken], [FlatToken]) deriving (Show)
newtype Meta = Meta (Title, Author) deriving (Show)

getTitle :: Meta -> [FlatToken]
getTitle (Meta (Title title, _)) = title

getAuthor :: Meta -> [FlatToken]
getAuthor (Meta (_, Author (author, _))) = author

getLitAuthor :: Meta -> [FlatToken]
getLitAuthor (Meta (_, Author (_, author))) = author
