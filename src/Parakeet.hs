module Parakeet (
  parakeet
, module Parakeet.Types.Options
, module Control.Monad.Parakeet
) where

import Control.Monad.Parakeet (Parakeet, runParakeet)
import Control.Monad.Reader (asks)
import Data.Text.Lazy (unpack)

import Parakeet.Parser.Parser (parse)
import Parakeet.Types.Options 
import Parakeet.Translator.Tex (tex, texBare)
import Parakeet.Translator.Intermediate (intermediate)

parakeet :: Parakeet String
parakeet = do
  format <- asks optOutput
  parsed <- parse
  unpack <$> translator format parsed
  where translator format = case format of
          InTex -> tex
          InBareTex -> texBare
          InIntermediate -> intermediate