module Token.Katakana (
  isNormal
, isSmall
, isSokuon
, isKatakana
) where

import           Control.Monad (guard, msum, mzero, join)
import           Data.Maybe (isJust)
import qualified Data.Map as M

import           Token.Token (TokenKana(..), wrap, unwrap, Katakana)
import qualified Token.Compound as C
import           Token.Romaji (otherForms, sokuonize, longVowelize, isSyllabicN, fromRomaji)
import           Token.Misc (isChoonpu)
import           Token.Internal (kRaw)
import           Monad.Choice (fromMaybe)

chmap :: M.Map String String
chmap = M.fromList kRaw

instance TokenKana Katakana where
  buildCompound k r = C.Katakana (unwrap k) (map unwrap r)

  toRomaji k =  lookup (unwrap k)
    where
      lookup [] = mzero
      lookup k | isSokuon (head k) = sokuonize <$> lookup (tail k)
               | isChoonpu (last k) = longVowelize False <$> lookup (init k)
               | otherwise = return <$> join (otherForms . wrap <$> fromMaybe (M.lookup k chmap))
  
  fromNRomaji r = sequence $ convert r
    where
      convert [] = []
      convert (x:xs) = msum (map (\f -> f x) [checkSyllabicN, lookupNormal, checkChoonpu]) : convert xs
        where
          checkSyllabicN x = do
            guard $ isSyllabicN x
            snd <$> fromRomaji (wrap "n")
          lookupNormal x = snd <$> fromRomaji x
          checkChoonpu x = do
            guard $ length x' == 1
            return $ wrap "ッ"
            where x' = unwrap x

isNormal :: Char -> Bool
isNormal = isJust . flip M.lookup chmap . return

isSmall :: Char -> Bool
isSmall c = c `elem` ['ァ', 'ィ', 'ゥ', 'ェ', 'ォ', 'ッ', 'ャ', 'ュ', 'ョ', 'ヮ', 'ヵ', 'ヶ']
    -- [0x30a1, 0x30a3, 0x30a5, 0x30a7, 0x30a9, 0x30c3, 0x30e3, 0x30e5, 0x30e7, 0x30ee, 0x30f5, 0x30f6]

isSokuon :: Char -> Bool  -- 片仮名促音
isSokuon = (==) 'ッ'

isKatakana :: Char -> Bool
isKatakana c = isNormal c || isSmall c

