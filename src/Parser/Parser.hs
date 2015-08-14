module Parser.Parser (
  doParse
) where

import           Text.Parsec
import           Text.Parsec.String
import           Text.Parsec.Combinator
import           Text.Parsec.Char
import           Text.Parsec.Pos

import           Control.Applicative ((<$>), (*>))
import           Control.Monad (void, guard, liftM, liftM2, liftM3)
import           Data.Char (toLower, toUpper, isSpace, isAlpha)
import           Data.List (sortBy, nub)
import           Data.Function (on)
import           Data.Maybe (fromJust, isJust)

import           Parser.Stage0 (stage0)
import           Parser.Stage1 (stage1)

import qualified Token.Token as T
import qualified Token.Hiragana as H
import qualified Token.Katakana as K
import qualified Token.Romaji as R
import qualified Token.Misc as M
import qualified TexElem as E

-- import System.IO.Unsafe

lowerCase :: String -> String
lowerCase = map toLower

setLine l = do
  pos <- getPosition
  setPosition $ setSourceLine pos l

parseLine :: Line -> String -> String -> Either ParseError [E.TexElem]
parseLine l j r = do
  wds <- runParser (setLine l >> stage0) () "Japanese" j 
  runParser (setLine l >> stage1) wds "Romaji" (lowerCase r)
  -- where evil = (unsafePerformIO . putStrLn . concatMap (\token -> (T.unwrapToken token) ++ " ")) wds

doParse :: String -> String -> [E.TexElem]
doParse j r = fromEither $ fmap concat $ sequence $ zipWith3 parseLine [1 .. ] (lines j) (lines r) 
  where
    fromEither (Left err) = error $ show err
    fromEither (Right va) = va
