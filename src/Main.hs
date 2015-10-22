module Main (main) where

import System.Environment (getArgs)
import Control.Monad.Reader (asks)
import Data.Text.Lazy (unpack)

import Options (Options(..), OutputFormat(..), runOpts)
import Parser.Parser (parse)
import Translator.Tex (tex, texBare)
import Translator.Intermediate (intermediate)
import Monad.Parakeet (Parakeet, runParakeet)

parakeet :: Parakeet String
parakeet = do
  format <- asks optOutput
  parsed <- parse
  unpack <$> translator format parsed
  where translator format = case format of
          InTex -> tex
          InBareTex -> texBare
          InIntermediate -> intermediate

main = do
  opts <- runOpts =<< getArgs
  let put = optOutputIO opts
  let res = runParakeet opts parakeet
  case res of 
    Left err -> fail err
    Right r  -> put r
