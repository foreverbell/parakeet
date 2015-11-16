module Data.Char.Extra (
  toLower
) where

import qualified Data.Char as C

toLower :: String -> String
toLower = map C.toLower
