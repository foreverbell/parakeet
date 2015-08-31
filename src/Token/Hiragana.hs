module Token.Hiragana (
  isNormal
, isSmall
, isSokuon
, isHiragana
) where

import           Control.Applicative ((<$>))
import           Control.Monad (guard, msum, mzero, join)
import           Data.Maybe (isJust)
import qualified Data.Map as M

import           Token.Token (TokenKana(..), wrap, unwrap, Hiragana)
import qualified Token.Compound as C
import           Token.Romaji (otherForms, sokuonize, isSyllabicN, fromRomaji)
import           Token.Internal (hRaw)
import           Monad.Choice (fromMaybe)

chmap :: M.Map String String
chmap = M.fromList hRaw

instance TokenKana Hiragana where
  buildCompound k r = C.Hiragana (unwrap k) (map unwrap r)

  toRomaji h = lookup (unwrap h)
    where
      lookup [] = mzero
      lookup h@(x:xs) | isSokuon x = sokuonize <$> lookup xs
                      | otherwise  = return <$> join (otherForms . wrap <$> fromMaybe (M.lookup h chmap))
  
  fromNRomaji r = sequence $ convert r
    where
      convert [] = []
      convert (x:xs) = msum (map (\f -> f x) [checkSyllabicN, lookupNormal, checkChoonpu]) : convert xs
        where
          checkSyllabicN x = do
            guard $ isSyllabicN x
            fst <$> fromRomaji (wrap "n")
          lookupNormal x = fst <$> fromRomaji x
          checkChoonpu x = do
            guard $ length x' == 1
            return $ wrap "っ"
            where x' = unwrap x

isNormal :: Char -> Bool
isNormal = isJust . flip M.lookup chmap . return

isSmall :: Char -> Bool
isSmall c = c `elem` ['ぁ', 'ぃ', 'ぅ', 'ぇ', 'ぉ', 'っ', 'ゃ', 'ゅ', 'ょ', 'ゎ']
    -- [0x3041, 0x3043, 0x3045, 0x3047, 0x3049, 0x3063, 0x3083, 0x3085, 0x3087, 0x308e, 0x3095, 0x3096] 
    -- last two (3095, 3096) aren't commonly used in modern Japanese (not displayable).

isSokuon :: Char -> Bool  -- 平仮名促音
isSokuon = (==) 'っ'

isHiragana :: Char -> Bool
isHiragana c = isNormal c || isSmall c

