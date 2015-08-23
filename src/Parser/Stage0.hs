module Parser.Stage0 (
  stage0
) where

import           Text.Parsec
import           Text.Parsec.String
import           Control.Applicative (Applicative, (<$>))
import           Control.Monad (liftM2)

import           Parser.Stage1 (AnyToken(..), TokenWrap)
import qualified Token.Token as T
import qualified Token.Hiragana as H
import qualified Token.Katakana as K
import qualified Token.Misc as M

concatM :: (Monad m) => m [a] -> m [a] -> m [a]
concatM = liftM2 (++)

(<&&>) :: (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool)
(<&&>) p1 p2 c = p1 c && p2 c

parseTwo n s = do
  first <- satisfy n
  option [first] $ do 
    second <- satisfy s
    return [first, second]

hiragana :: Parser T.Hiragana
hiragana = T.wrap <$> hSokuon `concatM` hMain
  where
    hSokuon  = option [] $ return <$> satisfy H.isSokuon
    hMain = parseTwo H.isNormal (H.isSmall <&&> (not . H.isSokuon))

katakana :: Parser T.Katakana
katakana = T.wrap <$> kSokuon `concatM` kMain `concatM` kChoonpu
  where
    kSokuon  = option [] $ return <$> satisfy K.isSokuon 
    kMain = parseTwo K.isNormal (K.isSmall <&&> (not . K.isSokuon))
    kChoonpu  = option [] $ return <$> satisfy M.isChoonpu

kanji :: Parser T.Kanji
kanji = T.wrap <$> many1 (satisfy M.isKanji)

lit :: Parser T.Lit
lit = T.wrap <$> many1 (satisfy other) 
  where 
    other c = not $ any (\f -> f c) 
                [ M.isChoonpu
                , M.isKanji, H.isHiragana, K.isKatakana ]

stage0 :: Parser [AnyToken]
stage0 = do
  r <- many $ choice $ [wrapAnyA hiragana, wrapAnyA katakana, wrapAnyA kanji, wrapAnyA lit]
  eof
  return $ r ++ [wrapAny (T.wrap "\n" :: T.Lit)]

wrapAny :: (TokenWrap t) => t -> AnyToken
wrapAny = AnyToken

wrapAnyA :: (Applicative f, TokenWrap t) => f t -> f AnyToken
wrapAnyA = fmap wrapAny

