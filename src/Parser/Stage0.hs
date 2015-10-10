module Parser.Stage0 (
  stage0
) where

import           Text.Parsec
import           Control.Monad (liftM2)

import           Parser.Stage1 (TokenBox(..))
import qualified Token.Token as T
import qualified Token.Hiragana as H
import qualified Token.Katakana as K
import qualified Token.Misc as M
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

hiragana :: Parser T.Hiragana
hiragana = T.wrap <$> hSokuon `concatM` hMain
  where
    hSokuon = option [] $ return <$> satisfy H.isSokuon
    hMain = parseTwo H.isNormal (H.isSmall `but` H.isSokuon)

katakana :: Parser T.Katakana
katakana = T.wrap <$> kSokuon `concatM` kMain `concatM` kChoonpu
  where
    kSokuon = option [] $ return <$> satisfy K.isSokuon 
    kMain = parseTwo K.isNormal (K.isSmall `but` K.isSokuon)
    kChoonpu  = option [] $ return <$> satisfy M.isChoonpu

kanji :: Parser T.Kanji
kanji = T.wrap <$> many1 (satisfy M.isKanji)

lit :: Parser T.Lit
lit = T.wrap <$> many1 (satisfy other <|> dsep) 
  where 
    other c = not $ any (\f -> f c) 
                [ M.isChoonpu
                , M.isKanji, H.isHiragana, K.isKatakana
                , M.isSeparator
                ]
    dsep = try $ do
      string $ replicate 2 M.separator
      return M.separator

separator :: Parser T.Separator
separator = do
  char M.separator
  notFollowedBy $ char M.separator
  return $ T.wrap []

stage0 :: Parser [TokenBox]
stage0 = do
  r <- many $ choice [ TokenBox <$> hiragana
                     , TokenBox <$> katakana
                     , TokenBox <$> kanji
                     , TokenBox <$> lit
                     , TokenBox <$> separator
                     ]
  eof
  return $ r ++ [TokenBox (T.wrap "\n" :: T.Lit)]
