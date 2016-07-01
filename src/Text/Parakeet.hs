{-# LANGUAGE QuasiQuotes #-}

module Text.Parakeet (
  parakeet
, templateTeX
, templateHTML
, OutputFormat (..)
, module Parakeet.Types.Options
) where

import Control.Monad.Parakeet (runParakeet, SomeException)
import Data.Text.Lazy (unpack)
import Text.QuasiEmbedFile (rfile)

import Parakeet.Parser.Parser (parse)
import Parakeet.Printer.TeX (tex)
import Parakeet.Printer.HTML (html)
import Parakeet.Types.Options 

data OutputFormat = TeXFormat | HTMLFormat

parakeet :: Options -> OutputFormat -> Either SomeException String
parakeet opts format = runParakeet opts $ do
  parsed <- parse
  let printer = case format of
                  TeXFormat -> tex
                  HTMLFormat -> html
  unpack <$> printer parsed

templateTeX :: String
templateTeX = [rfile|template.tex|]

templateHTML :: String
templateHTML = [rfile|template.html|]
