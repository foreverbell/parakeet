module Parser (
  parseDoc
) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char
import Control.Applicative ((<$>))
import Control.Monad (liftM, liftM2, liftM3)
import Data.Char (isSpace)

import qualified JPToken as T
import qualified TexElem as E

import Debug.Trace
import System.IO.Unsafe

-- * Japanese parsing

hika :: Parser String
hika = do
      liftM2 (++) hSokuon hirigana
  <|> liftM3 cat3 kSokuon katahana choonpu
  where
    c2s c      = [c]
    cat3 a b c = a ++ b ++ c
    (&.) p1 p2 = \c -> (p1 c) && (p2 c)
    parse2 n s = do
      first <- satisfy n
      option [first] $ do 
        second <- satisfy s
        return [first, second]
    hSokuon  = option [] $ c2s <$> satisfy T.isHiraganaSokuon
    kSokuon  = option [] $ c2s <$> satisfy T.isKatakanaSokuon 
    hirigana = parse2 T.isHiraganaNormal (T.isHiraganaSmall &. (not . T.isHiraganaSokuon))
    katahana = parse2 T.isKatakanaNormal (T.isKatakanaSmall &. (not . T.isKatakanaSokuon))
    choonpu  = option [] $ c2s <$> satisfy T.isChoonpu

kanji :: Parser String
kanji = many1 $ satisfy T.isKanji

lit :: Parser String
lit = many1 $ satisfy other
  where 
    other c | crlf c    = False
            | otherwise = not $ or $ map (\f -> f c) 
                            [ T.isChoonpu
                            , T.isKanji, T.isHiragana, T.isKatakana ]
    crlf c = c == '\r' || c == '\n'

parseLine :: Parser [String]
parseLine = do
  r <- many (choice [hika, kanji, lit])
  optional newline
  return $ r ++ ["\n"]

parseJap :: Parser [String]
parseJap = concat <$> manyTill parseLine eof

-- * Romaji parsing

type Parser2 = Parsec String [String]

parseRoj :: Parser2 [E.TexElem]
parseRoj = undefined

-- * Document parsing

parseDoc :: String -> String -> [E.TexElem]
parseDoc j r = unsafePerformIO (ff parsed) `seq` undefined
  where
    parsed = parse parseJap [] j

ff (Right xs) = putStrLn $ concatMap (++ " ") xs
