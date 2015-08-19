module Parser.Stage0 (
  stage0
) where

import           Text.Parsec
import           Text.Parsec.String
import           Control.Applicative ((<$>))
import           Control.Monad (liftM2)

import qualified Token.Token as T
import qualified Token.Hiragana as H
import qualified Token.Katakana as K
import qualified Token.Misc as M

liftCat :: (Monad m) => m [a] -> m [a] -> m [a]
liftCat = liftM2 (++)

(&.) p1 p2 c = p1 c && p2 c

hika :: Parser T.Token
hika = T.Hiragana <$> hSokuon `liftCat` hirigana
   <|> T.Katakana <$> kSokuon `liftCat` katahana `liftCat` choonpu
  where
    parse2 n s = do
      first <- satisfy n
      option [first] $ do 
        second <- satisfy s
        return [first, second]
    hSokuon  = option [] $ return <$> satisfy H.isSokuon
    kSokuon  = option [] $ return <$> satisfy K.isSokuon 
    hirigana = parse2 H.isNormal (H.isSmall &. (not . H.isSokuon))
    katahana = parse2 K.isNormal (K.isSmall &. (not . K.isSokuon))
    choonpu  = option [] $ return <$> satisfy M.isChoonpu

kanji :: Parser T.Token
kanji = T.Kanji <$> many1 (satisfy M.isKanji)

lit :: Parser T.Token
lit = T.Lit <$> many1 (satisfy other)
  where 
    other c = not $ any (\f -> f c) 
                [ M.isChoonpu
                , M.isKanji, H.isHiragana, K.isKatakana ]

stage0 :: Parser [T.Token]
stage0 = do
  r <- many (choice [hika, kanji, lit])
  eof
  return $ r ++ [T.Lit "\n"]
