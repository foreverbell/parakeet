module Parser.Parser (
  doParse
) where

import           Text.Parsec
import           Data.Char (toLower)

import           Parser.Stage0 (stage0)
import           Parser.Stage1 (stage1)

import           Options (Options(..))
import qualified Element as E

-- import System.IO.Unsafe

lowerCase :: String -> String
lowerCase = map toLower

setLine l = do
  pos <- getPosition
  setPosition $ setSourceLine pos l

parseLine :: Options -> Line -> String -> String -> Either ParseError [E.Element]
parseLine opts l j r = do
  wds <- runParser (setLine l >> stage0) () (optJInputFile opts) j 
  runParser (setLine l >> stage1) wds (optRInputFile opts) (lowerCase r)
  -- where evil = (unsafePerformIO . putStrLn . concatMap (\token -> (T.unwrapToken token) ++ " ")) wds

doParse :: Options -> String -> String -> [E.Element]
doParse opts j r = fromEither $ fmap concat $ sequence $ zipWith3 (parseLine opts) [1 .. ] (lines j) (lines r) 
  where
    fromEither (Left err) = error $ show err
    fromEither (Right va) = va
