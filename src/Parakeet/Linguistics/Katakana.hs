module Parakeet.Linguistics.Katakana (
  isNormal
, isSmall
, isSokuon
, isIterationMark
, isKatakana
) where

import           Control.Monad (guard, msum, mzero, join)
import           Control.Monad.Choice (fromMaybe, toMaybe)
import           Data.Maybe (isJust)
import qualified Data.Map as M

import           Parakeet.Types.Lexeme (LexemeKana(..), wrap, unwrap, Katakana)
import           Parakeet.Linguistics.Romaji (otherForms, dakutenize, unDakutenize, sokuonize, longVowelize, isSyllabicN, toKana)
import           Parakeet.Linguistics.Misc (isChoonpu)
import           Parakeet.Linguistics.Internal (kRaw)

chmap :: M.Map String String
chmap = M.fromList kRaw

instance LexemeKana Katakana where
  toRomaji k = lookup (unwrap k)
    where
      lookup [] = mzero
      lookup k | isSokuon (head k) = sokuonize <$> lookup (tail k)
               | isChoonpu (last k) = longVowelize <$> lookup (init k)
               | isIterationMark1 (last k) = (\xs -> xs ++ [unDakutenize (last xs)]) <$> lookup (init k)
               | isIterationMark2 (last k) = (\xs -> xs ++ [dakutenize (last xs)]) <$> lookup (init k)
               | otherwise = return <$> join (otherForms . wrap <$> fromMaybe (M.lookup k chmap))
  
  fromRomaji ks = toMaybe <$> convert ks
    where
      convert [] = []
      convert (x:xs) = msum (map (\f -> f x) [checkSyllabicN, lookupNormal, checkSokuon]) : convert xs
        where
          checkSyllabicN x = do
            guard $ isSyllabicN x
            snd $ toKana (wrap "n")
          lookupNormal x = snd $ toKana x
          checkSokuon x = do
            guard $ length x' == 1
            return $ wrap [sokuon]
            where x' = unwrap x

sokuon :: Char
sokuon = 'ッ'

iterationMarks :: String
iterationMarks = ['ヽ', 'ヾ']

isNormal :: Char -> Bool
isNormal = isJust . flip M.lookup chmap . return

isSmall :: Char -> Bool
isSmall c = c `elem` ['ァ', 'ィ', 'ゥ', 'ェ', 'ォ', 'ッ', 'ャ', 'ュ', 'ョ', 'ヮ', 'ヵ', 'ヶ']

isSokuon :: Char -> Bool
isSokuon = (==) sokuon

isIterationMark :: Char -> Bool
isIterationMark c = c `elem` iterationMarks

isIterationMark1 :: Char -> Bool
isIterationMark1 c = c == iterationMarks !! 0

isIterationMark2 :: Char -> Bool
isIterationMark2 c = c == iterationMarks !! 1

isKatakana :: Char -> Bool
isKatakana c = isNormal c || isSmall c || isIterationMark c
