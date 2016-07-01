module Parakeet.Types.Document (
  Document (..)
, Meta (..)
) where

import Parakeet.Types.Token2 (Token2)

data Meta = Meta {
  title :: String
, author :: String
} deriving (Show)

data Document = Document {
  metaInfo :: Maybe Meta
, body :: [Token2]
} deriving (Show)
