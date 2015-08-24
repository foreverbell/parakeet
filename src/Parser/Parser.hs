module Parser.Parser (
  parse
) where

import           Text.Parsec hiding (parse)
import           Control.Monad.Reader (asks)
import           Control.Monad.Error (throwError)
import           Control.Applicative ((<$>))
import           Data.Char (toLower)

import           Eval (Eval)
import           Options (Options(..))
import           Parser.Stage0 (stage0)
import           Parser.Stage1 (stage1)
import qualified Element as E

setLine l = do
  pos <- getPosition
  setPosition $ setSourceLine pos l

parseLine :: Line -> String -> String -> Eval [E.Element]
parseLine l j r = do
  jf <- asks optJInputFile
  rf <- asks optRInputFile
  case p jf rf of
    Left err -> throwError (show err)
    Right es -> return es
  where
    p jf rf = do wd <- runParser (setLine l >> stage0) () jf j 
                 runParser (setLine l >> stage1) wd rf (map toLower r)

parse :: Eval [E.Element]
parse = do
  (j, r) <- asks optContent
  concat <$> sequence (zipWith3 parseLine [1 .. ] (lines j) (lines r))

