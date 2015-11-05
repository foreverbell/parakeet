module Linguistics.Hiragana (
  isNormal
, isSmall
, isSokuon
, isIterationMark
, isHiragana
) where

import           Control.Monad (guard, msum, mzero, join)
import           Data.Maybe (isJust)
import qualified Data.Map as M

import           Linguistics.Lexeme (LexemeKana(..), wrap, unwrap, Hiragana)
import           Linguistics.Romaji (otherForms, sokuonize, isSyllabicN, toKana)
import           Linguistics.Internal (hRaw)
import           Monad.Choice (fromMaybe, toMaybe)

chmap :: M.Map String String
chmap = M.fromList hRaw

instance LexemeKana Hiragana where
  toRomaji h = lookup (unwrap h)
    where
      lookup [] = mzero
      lookup h@(x:xs) | isSokuon x = sokuonize <$> lookup xs
                      | otherwise  = return <$> join (otherForms . wrap <$> fromMaybe (M.lookup h chmap))
  
  fromRomaji hs = toMaybe <$> convert hs
    where
      convert [] = []
      convert (x:xs) = msum (map (\f -> f x) [checkSyllabicN, lookupNormal, checkChoonpu]) : convert xs
        where
          checkSyllabicN x = do
            guard $ isSyllabicN x
            fst $ toKana (wrap "n")
          lookupNormal x = fst $ toKana x
          checkChoonpu x = do
            guard $ length x' == 1
            return $ wrap [sokuon]
            where x' = unwrap x

sokuon :: Char
sokuon = 'っ'

iterationMark :: Char
iterationMark = 'ゝ'

isNormal :: Char -> Bool
isNormal = isJust . flip M.lookup chmap . return

isSmall :: Char -> Bool
isSmall c = c `elem` ['ぁ', 'ぃ', 'ぅ', 'ぇ', 'ぉ', 'っ', 'ゃ', 'ゅ', 'ょ', 'ゎ', 'ゕ', 'ゖ']

isSokuon :: Char -> Bool
isSokuon = (==) sokuon

isIterationMark :: Char -> Bool
isIterationMark = (==) iterationMark

isHiragana :: Char -> Bool
isHiragana c = isNormal c || isSmall c || isIterationMark c
