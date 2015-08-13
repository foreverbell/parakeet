module Parser.Parser (
  doParse
) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char

import Control.Applicative ((<$>), (*>))
import Control.Monad (void, guard, liftM, liftM2, liftM3)
import Data.Char (toLower, toUpper, isSpace, isAlpha)
import Data.List (sortBy, nub)
import Data.Function (on)
import Data.Maybe (fromJust, isJust)

import Parser.Stage0 (stage0)
import Parser.Stage1 (stage1)

import qualified Token.Token as T
import qualified Token.Hiragana as H
import qualified Token.Katakana as K
import qualified Token.Romaji as R
import qualified Token.Misc as M
import qualified TexElem as E

-- import System.IO.Unsafe

lowerCase :: String -> String
lowerCase = map toLower

parseLine :: String -> String -> Either ParseError [E.TexElem]
parseLine j r = do
  wds <- runParser stage0 () [] j 
  runParser stage1 wds [] (lowerCase r)
  -- where evil = (unsafePerformIO . putStrLn . concatMap (\token -> (T.unwrapToken token) ++ " ")) wds

doParse :: String -> String -> [E.TexElem]
doParse j r = fromEither $ fmap concat $ sequence $ zipWith parseLine (lines j) (lines r) 
  where
    fromEither (Left err) = error $ show err
    fromEither (Right va) = va


