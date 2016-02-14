{-# LANGUAGE QuasiQuotes #-}

module Text.Parakeet (
  parakeet
, template
, module Parakeet.Types.Options
) where

import Control.Monad.Parakeet (runParakeet, SomeException)
import Data.Text.Lazy (unpack)
import Text.QuasiEmbedFile (rfile)

import Parakeet.Parser.Parser (parse)
import Parakeet.Parser.TeX (tex)
import Parakeet.Types.Options 

parakeet :: Options -> Either SomeException String
parakeet opts = runParakeet opts $ do
  parsed <- parse
  unpack <$> tex parsed

template :: String
template = [rfile|template.tex|]
