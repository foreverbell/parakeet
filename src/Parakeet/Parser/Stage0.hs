module Parakeet.Parser.Stage0 (
  stage0
) where

import           Text.Parsec
import           Control.Monad (liftM2, void)
import           Control.Monad.Parakeet 

import           Parakeet.Parser.Stage1 (TokenBox(..))
import qualified Parakeet.Linguistics.Lexeme as L
import qualified Parakeet.Linguistics.Hiragana as H
import qualified Parakeet.Linguistics.Katakana as K
import qualified Parakeet.Linguistics.Misc as M

type Parser = ParsecT String () Parakeet

concatM :: Monad m => [m [a]] -> m [a]
concatM xs = foldl (liftM2 (++)) (return []) xs

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
kanji = L.wrap <$> many1 (satisfy M.isKanji)

lit :: Parser L.Lit
lit = L.wrap <$> many1 (satisfy other <|> dsep)
  where 
    other c = not $ any (\f -> f c) 
                [ M.isChoonpu
                , M.isKanji, H.isHiragana, K.isKatakana
                , M.isSeparator
                ]
    dsep = try $ do
      string $ replicate 2 M.separator
      return M.separator

separator :: Parser ()
separator = try $ void $ do
  char M.separator
  notFollowedBy $ char M.separator

stage0 :: Parser [TokenBox]
stage0 = do
  r <- many $ optional separator >> choice [ TokenBox <$> hiragana
                                           , TokenBox <$> katakana
                                           , TokenBox <$> kanji
                                           , TokenBox <$> lit
                                           ]
  eof
  return $ r ++ [TokenBox (L.wrap "\n" :: L.Lit)]
