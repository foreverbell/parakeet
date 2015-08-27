module Parser.Parser (
  parse
) where

import Text.Parsec hiding (parse)
import Control.Applicative ((<$>))
import Control.Monad.Reader (asks)
import Control.Monad.Error (throwError)
import Data.Char (toLower)

import Eval (Eval)
import Options (Options(..))
import Parser.Stage0 (stage0)
import Parser.Stage1 (stage1)
import Token.Compound (Compound(..))

setLine l = do
  pos <- getPosition
  setPosition $ setSourceLine pos l

parseLine :: Line -> String -> String -> Eval [Compound]
parseLine l j r = do
  jf <- asks optJInputFile
  rf <- asks optRInputFile
  wd <- test =<< runParserT (setLine l >> stage0) () jf j
  test =<< runParserT (setLine l >> stage1) wd rf (map toLower r)
  where test either = case either of
          Left err -> throwError (show err)
          Right es -> return $ es

parse :: Eval [Compound]
parse = do
  (j, r) <- asks optContent
  concat <$> sequence (zipWith3 parseLine [1 .. ] (lines j) (lines r))
