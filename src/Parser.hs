module Parser (
  parseDoc
) where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>))
import Control.Monad (liftM, liftM2, liftM3)
import Data.Char (isSpace)

import qualified JPToken as JT
import qualified ArticleDoc as AD

import Debug.Trace
import System.IO.Unsafe

hika :: Parser String
hika = do
      liftM2 (++) hSokuon hirigana
  <|> liftM3 cat3 kSokuon katahana choonpu
  where
    c2s c = [c]
    cat3 a b c = a ++ b ++ c
    parse2 n s = do
      first <- satisfy n
      option [first] $ do 
        second <- satisfy s
        return [first, second]
    hSokuon  = option [] $ c2s <$> satisfy JT.isHSokuon
    kSokuon  = option [] $ c2s <$> satisfy JT.isKSokuon 
    hirigana = parse2 JT.isHiraganaNormal JT.isHiraganaSmall
    katahana = parse2 JT.isKatakanaNormal JT.isKatakanaSmall
    choonpu  = option [] $ c2s <$> satisfy JT.isChoonpu

kanji :: Parser String
kanji = many1 $ satisfy JT.isKanji

lit :: Parser String
lit = many1 $ satisfy other
  where 
    other c | crlf c    = False
            | otherwise = not $ or $ map (\f -> f c) 
                            [ JT.isChoonpu, JT.isHSokuon, JT.isKSokuon
                            , JT.isKanji
                            , JT.isHiraganaNormal, JT.isHiraganaSmall
                            , JT.isKatakanaNormal, JT.isKatakanaSmall ]
    crlf c = c == '\r' || c == '\n'

parseLine :: Parser [String]
parseLine = do
  r <- many (choice [hika, kanji, lit])
  optional newline
  return r

parseJap :: Parser [String]
parseJap = concat <$> manyTill parseLine eof

parseDoc :: String -> String -> AD.ArticleDoc
parseDoc j r = unsafePerformIO (ff parsed) `seq` undefined
  where
    parsed = runParser parseJap () [] j

ff (Right xs) = putStrLn $ concatMap (++ " ") xs
