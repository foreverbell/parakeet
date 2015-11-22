module Main (
  main
) where

import Control.Monad.Parakeet (Parakeet, runParakeet)
import Control.Monad.Reader (asks)
import Data.Text.Lazy (unpack)
import System.Environment (getArgs)

import Parakeet.Types.Options (Options(..), OutputFormat(..))
import Parakeet.Parser.Parser (parse)
import Parakeet.Translator.Tex (tex, texBare)
import Parakeet.Translator.Intermediate (intermediate)
import Parakeet.Options (runOpts)

parakeet :: Parakeet String
parakeet = do
  format <- asks optOutput
  parsed <- parse
  unpack <$> translator format parsed
  where translator format = case format of
          InTex -> tex
          InBareTex -> texBare
          InIntermediate -> intermediate

main :: IO ()
main = do
  opts <- runOpts =<< getArgs
  let put = optOutputIO opts
  let res = runParakeet opts parakeet
  case res of 
    Left err -> fail err
    Right r  -> put r
