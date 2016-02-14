module Parakeet.Linguistics.Hiragana (
  isNormal
, isSmall
, isSokuon
, isIterationMark
, isHiragana
) where

import           Control.Monad (guard, msum, mzero, join)
import           Control.Monad.Choice (fromMaybe, toMaybe)
import           Data.Maybe (isJust)
import qualified Data.Map as M

import           Parakeet.Types.Lexeme (LexemeKana (..), wrap, unwrap, Hiragana)
import           Parakeet.Linguistics.Romaji (otherForms, dakutenize, unDakutenize, sokuonize, isSyllabicN, toKana)
import           Parakeet.Linguistics.RawData (hRaw)

chmap :: M.Map String String
chmap = M.fromList hRaw

instance LexemeKana Hiragana where
  toRomaji h = lookup (unwrap h)
    where
      lookup [] = mzero
      lookup h | isSokuon (head h) = sokuonize <$> lookup (tail h)
               | isIterationMark1 (last h) = (\xs -> xs ++ [unDakutenize (last xs)]) <$> lookup (init h)
               | isIterationMark2 (last h) = (\xs -> xs ++ [dakutenize (last xs)]) <$> lookup (init h)
               | otherwise  = return <$> join (otherForms . wrap <$> fromMaybe (M.lookup h chmap))
  
  fromRomaji hs = toMaybe <$> convert hs
    where
      convert [] = []
      convert (x:xs) = msum (map ($ x) [checkSyllabicN, lookupNormal, checkSokuon]) : convert xs
        where
          checkSyllabicN x = do
            guard $ isSyllabicN x
            fst $ toKana (wrap "n")
          lookupNormal x = fst $ toKana x
          checkSokuon x = do
            guard $ length x' == 1
            return $ wrap [sokuon]
            where x' = unwrap x

sokuon :: Char
sokuon = 'っ'

iterationMarks :: String
iterationMarks = ['ゝ', 'ゞ']

isNormal :: Char -> Bool
isNormal = isJust . flip M.lookup chmap . return

isSmall :: Char -> Bool
isSmall = flip elem ['ぁ', 'ぃ', 'ぅ', 'ぇ', 'ぉ', 'っ', 'ゃ', 'ゅ', 'ょ', 'ゎ', 'ゕ', 'ゖ']

isSokuon :: Char -> Bool
isSokuon = (==) sokuon

isIterationMark :: Char -> Bool
isIterationMark = flip elem iterationMarks

isIterationMark1 :: Char -> Bool
isIterationMark1 = (==) (iterationMarks !! 0)

isIterationMark2 :: Char -> Bool
isIterationMark2 = (==) (iterationMarks !! 1)

isHiragana :: Char -> Bool
isHiragana c = isNormal c || isSmall c || isIterationMark c
