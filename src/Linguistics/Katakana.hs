module Linguistics.Katakana (
  isNormal
, isSmall
, isSokuon
, isKatakana
) where

import           Control.Monad (guard, msum, mzero, join)
import           Data.Maybe (isJust)
import qualified Data.Map as M

import           Linguistics.Lexeme (LexemeKana(..), wrap, unwrap, Katakana)
import           Linguistics.Romaji (otherForms, sokuonize, longVowelize, isSyllabicN, toKana)
import           Linguistics.Misc (isChoonpu)
import           Linguistics.Internal (kRaw)
import           Monad.Choice (fromMaybe)

chmap :: M.Map String String
chmap = M.fromList kRaw

instance LexemeKana Katakana where
  toRomaji k = lookup (unwrap k)
    where
      lookup [] = mzero
      lookup k | isSokuon (head k) = sokuonize <$> lookup (tail k)
               | isChoonpu (last k) = longVowelize False <$> lookup (init k)
               | otherwise = return <$> join (otherForms . wrap <$> fromMaybe (M.lookup k chmap))
  
  fromRomaji = convert
    where
      convert [] = []
      convert (x:xs) = msum (map (\f -> f x) [checkSyllabicN, lookupNormal, checkChoonpu]) : convert xs
        where
          checkSyllabicN x = do
            guard $ isSyllabicN x
            snd $ toKana (wrap "n")
          lookupNormal x = snd $ toKana x
          checkChoonpu x = do
            guard $ length x' == 1
            return $ wrap [sokuon]
            where x' = unwrap x

sokuon :: Char
sokuon = 'ッ'

isNormal :: Char -> Bool
isNormal = isJust . flip M.lookup chmap . return

isSmall :: Char -> Bool
isSmall c = c `elem` ['ァ', 'ィ', 'ゥ', 'ェ', 'ォ', 'ッ', 'ャ', 'ュ', 'ョ', 'ヮ', 'ヵ', 'ヶ']
    -- [0x30a1, 0x30a3, 0x30a5, 0x30a7, 0x30a9, 0x30c3, 0x30e3, 0x30e5, 0x30e7, 0x30ee, 0x30f5, 0x30f6]

isSokuon :: Char -> Bool
isSokuon = (==) sokuon

isKatakana :: Char -> Bool
isKatakana c = isNormal c || isSmall c
