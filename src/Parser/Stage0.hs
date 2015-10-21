module Parser.Stage0 (
  stage0
) where

import           Text.Parsec
import           Control.Monad (liftM2, void)

import           Parser.Stage1 (TokenBox(..))
import qualified Linguistics.Lexeme as L
import qualified Linguistics.Hiragana as H
import qualified Linguistics.Katakana as K
import qualified Linguistics.Misc as M
import           Monad.Parakeet 

type Parser = ParsecT String () Parakeet

concatM :: (Monad m) => m [a] -> m [a] -> m [a]
concatM = liftM2 (++)

but :: (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool)
but p1 p2 c = p1 c && not (p2 c)

parseTwo :: (Char -> Bool) -> (Char -> Bool) -> Parser String
parseTwo n s = do
  first <- satisfy n
  option [first] $ do 
    second <- satisfy s
    return [first, second]

hiragana :: Parser L.Hiragana
hiragana = L.wrap <$> hSokuon `concatM` hMain
  where
    hSokuon = option [] $ return <$> satisfy H.isSokuon
    hMain = parseTwo H.isNormal (H.isSmall `but` H.isSokuon)

katakana :: Parser L.Katakana
katakana = L.wrap <$> kSokuon `concatM` kMain `concatM` kChoonpu
  where
    kSokuon = option [] $ return <$> satisfy K.isSokuon 
    kMain = parseTwo K.isNormal (K.isSmall `but` K.isSokuon)
    kChoonpu  = option [] $ return <$> satisfy M.isChoonpu

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
