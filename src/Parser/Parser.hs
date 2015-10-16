module Parser.Parser (
  parse
) where

import Text.Parsec hiding (parse)
import Control.Monad.Reader (asks)
import Control.Monad.Except (throwError)
import Data.Char (toLower)

import Monad.Parakeet (Parakeet)
import Options (Options(..))
import Parser.Stage0 (stage0)
import Parser.Stage1 (stage1)
import Parser.Token (Token(..))

setLine l = do
  pos <- getPosition
  setPosition $ setSourceLine pos l

parseLine :: Line -> String -> String -> Parakeet [Token]
parseLine l j r = do
  jf <- asks optJInputFile
  rf <- asks optRInputFile
  wd <- test =<< runParserT (setLine l >> stage0) () jf j
  test =<< runParserT (setLine l >> stage1) wd rf (map toLower r)
  where test = either (throwError . show) return

parse :: Parakeet [Token]
parse = do
  (j, r) <- asks optContent
  concat <$> sequence (zipWith3 parseLine [1 .. ] (lines j) (lines r))
