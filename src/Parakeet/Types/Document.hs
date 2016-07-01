module Parakeet.Types.Document (
  Document (..)
, Meta (..)
) where

import Parakeet.Types.FToken (FToken)

data Meta = Meta {
  title :: String
, author :: String
} deriving (Show)

data Document = Document {
  metaInfo :: Maybe Meta
, body :: [FToken]
} deriving (Show)
