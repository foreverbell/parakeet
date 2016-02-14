{-# LANGUAGE QuasiQuotes #-}

module Text.Parakeet (
  parakeet
, template
, module Parakeet.Types.Options
) where

import Control.Monad.Parakeet (env, runParakeet, SomeException)
import Data.Text.Lazy (unpack)
import Text.QuasiEmbedFile (rfile)

import Parakeet.Parser.Parser (parse)
import Parakeet.Types.Options 
import Parakeet.Translator.Tex (tex, plainTex)

parakeet :: Options -> Either SomeException String
parakeet opts = runParakeet opts $ do
  format <- env optOutput
  parsed <- parse
  unpack <$> translator format parsed
  where 
    translator InTex = tex
    translator InPlainTex = plainTex

template :: String
template = [rfile|template.tex|]
