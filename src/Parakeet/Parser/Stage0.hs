module Parakeet.Parser.Stage0 (
  stage0
) where

import           Control.Monad (void, liftM2)
import           Control.Monad.Extra (concatM)
import           Control.Monad.Parakeet 
import           Data.Char.Fuzzy (fuzzyAlphaNum)
import           Text.Parsec

import qualified Parakeet.Types.Lexeme as L
import qualified Parakeet.Linguistics.Hiragana as H
import qualified Parakeet.Linguistics.Katakana as K
import qualified Parakeet.Linguistics.Misc as M

type Parser = ParsecT String () Parakeet

but :: (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool)
but p1 p2 c = p1 c && not (p2 c)

parse2 :: (Char -> Bool) -> (Char -> Bool) -> Parser String
parse2 n s = do
  first <- satisfy n
  option [first] $ do 
    second <- satisfy s
    return [first, second]

hiragana :: Parser L.Hiragana
hiragana = L.wrap <$> concatM [sokuon, body, itermark]
  where
    sokuon = option [] $ return <$> satisfy H.isSokuon
    body = parse2 H.isNormal (H.isSmall `but` H.isSokuon)
    itermark = option [] $ return <$> satisfy H.isIterationMark

katakana :: Parser L.Katakana
katakana = L.wrap <$> concatM [sokuon, body, choonpu <|> itermark]
  where
    sokuon = option [] $ return <$> satisfy K.isSokuon 
    body = parse2 K.isNormal (K.isSmall `but` K.isSokuon)
    choonpu = option [] $ return <$> satisfy M.isChoonpu
    itermark = option [] $ return <$> satisfy K.isIterationMark

kanji :: Parser L.Kanji
kanji = L.wrap . concat <$> many1 (case1 <|> case2)
  where
    sokuon = satisfy H.isSokuon <|> satisfy K.isSokuon
    body = satisfy M.isKanji
    case1 = return <$> body
    case2 = try $ liftM2 (\a b -> [a, b]) sokuon body

alphanum :: Parser L.AlphaNum
alphanum = L.wrap <$> many1 (satisfy fuzzyAlphaNum)

lit :: Parser L.Lit
lit = L.wrap <$> many1 (satisfy other <|> escapedSeparator)
  where 
    other c = not $ any ($ c) 
                [ M.isChoonpu
                , M.isKanji, H.isHiragana, K.isKatakana
                , M.isSeparator
                , fuzzyAlphaNum
                ]
    escapedSeparator = try $ do -- use double separators to escape separator
      string $ replicate 2 M.separator
      return M.separator

separator :: Parser ()
separator = try $ void $ do
  char M.separator
  notFollowedBy $ char M.separator

stage0 :: Parser [L.SomeLexeme]
stage0 = do
  r <- many $ optional separator >> choice [ L.SomeLexeme <$> hiragana
                                           , L.SomeLexeme <$> katakana
                                           , L.SomeLexeme <$> kanji
                                           , L.SomeLexeme <$> alphanum
                                           , L.SomeLexeme <$> lit
                                           ]
  eof
  return $ r ++ [L.SomeLexeme (L.wrap "\n" :: L.Lit)]
