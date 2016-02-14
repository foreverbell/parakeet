module Parakeet.Types.Meta (
  Author (..)
, Title (..)
, Meta (..)
, getTitle
, getAuthor
, getLitAuthor
) where

import Parakeet.Types.FToken (FToken)

newtype Title = Title [FToken] deriving (Show)
newtype Author = Author ([FToken], [FToken]) deriving (Show)
newtype Meta = Meta (Title, Author) deriving (Show)

getTitle :: Meta -> [FToken]
getTitle (Meta (Title title, _)) = title

getAuthor :: Meta -> [FToken]
getAuthor (Meta (_, Author (author, _))) = author

getLitAuthor :: Meta -> [FToken]
getLitAuthor (Meta (_, Author (_, author))) = author
