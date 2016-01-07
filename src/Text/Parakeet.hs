module Text.Parakeet (
  parakeet
, module Parakeet.Types.Options
) where

import Control.Monad.Parakeet (env, runParakeet)
import Data.Text.Lazy (unpack)

import Parakeet.Parser.Parser (parse)
import Parakeet.Types.Options 
import Parakeet.Translator.Tex (tex, texBare)
import Parakeet.Translator.Intermediate (intermediate)

parakeet :: Options -> Either String String
parakeet opts = runParakeet opts $ do
  format <- env optOutput
  parsed <- parse
  unpack <$> translator format parsed
  where 
    translator format = case format of
      InTex -> tex
      InBareTex -> texBare
      InIntermediate -> intermediate
