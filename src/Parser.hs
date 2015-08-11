module Parser (
  parseDoc
) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char
import Control.Applicative ((<$>))
import Control.Monad (liftM, liftM2, liftM3)
import Data.Char (toLower)

import qualified Token.Token as T
import qualified Token.Hiragana as H
import qualified Token.Katakana as K
import qualified TexElem as E

import Debug.Trace
import System.IO.Unsafe

-- * Japanese parsing

type ParserJ = Parser

hikaJ :: ParserJ T.Token
hikaJ = do
      T.Hiragana <$> liftM2 (++) hSokuon hirigana
  <|> T.Katakana <$> liftM3 cat3 kSokuon katahana choonpu
  where
    c2s c      = [c]
    cat3 a b c = a ++ b ++ c
    (&.) p1 p2 = \c -> (p1 c) && (p2 c)
    parse2 n s = do
      first <- satisfy n
      option [first] $ do 
        second <- satisfy s
        return [first, second]
    hSokuon  = option [] $ c2s <$> satisfy H.isSokuon
    kSokuon  = option [] $ c2s <$> satisfy K.isSokuon 
    hirigana = parse2 H.isNormal (H.isSmall &. (not . H.isSokuon))
    katahana = parse2 K.isNormal (K.isSmall &. (not . K.isSokuon))
    choonpu  = option [] $ c2s <$> satisfy T.isChoonpu

kanjiJ :: ParserJ T.Token
kanjiJ = T.Kanji <$> many1 (satisfy T.isKanji)

litJ :: ParserJ T.Token
litJ = T.Lit <$> many1 (satisfy other)
  where 
    other c | crlf c    = False
            | otherwise = not $ or $ map (\f -> f c) 
                            [ T.isChoonpu
                            , T.isKanji, H.isHiragana, K.isKatakana ]
    crlf c = c == '\r' || c == '\n'

parseLineJ :: ParserJ [T.Token]
parseLineJ = do
  r <- many (choice [hikaJ, kanjiJ, litJ])
  optional newline
  return $ r ++ [T.Lit "\n"]

parseJap :: ParserJ [T.Token]
parseJap = concat <$> manyTill parseLineJ eof

-- * Romaji parsing

type ParserR = Parsec String [T.Token]

hikaR :: ParserR [T.Token]
hikaR = undefined

parseRoj :: ParserR [E.TexElem]
parseRoj = undefined

-- * Document parsing

parseDoc :: String -> String -> [E.TexElem]
parseDoc j r = evil `seq` fromEither $ runParser parseRoj wds [] r
  where
    fromEither (Left err) = error $ show err
    fromEither (Right va) = va
    wds = fromEither (parse parseJap [] j)
    evil = (unsafePerformIO . putStrLn . concatMap (\token -> (unwrap token) ++ " ")) wds

unwrap (T.Kanji s) = s
unwrap (T.Hiragana s) = s
unwrap (T.Katakana s) = s
unwrap (T.Romaji s) = s
unwrap (T.Lit s) = s
