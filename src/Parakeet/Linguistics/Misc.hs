module Parakeet.Linguistics.Misc (
  isKanji
, isChoonpu
, isMacron
, fromMacron
, toMacron
, toMacron'
, isVowel
, separator
, isSeparator
) where

import Data.Char (ord)
import Data.List (find)
import Control.Monad.Choice

isKanji :: Char -> Bool
isKanji = (\x -> (x >= 0x4e00 && x <= 0x9fbf) || x == k) . ord
  where k = ord '々'

isChoonpu :: Char -> Bool 
isChoonpu = (==) 'ー'

macrons :: [(Choice Char, Char)]
macrons = [ (fromList "āâ", 'a')
          , (fromList "īî", 'i')
          , (fromList "ūû", 'u')
          , (fromList "ēê", 'e')
          , (fromList "ōô", 'o')
          ]

isMacron :: Char -> Bool
isMacron c = c `elem` concatMap (toList . fst) macrons

fromMacron :: Char -> Char
fromMacron c = case find (\(m, _) -> c `elem` toList m) macrons of 
  Just (_, b) -> b
  Nothing -> c

toMacron :: Char -> Choice Char
toMacron c = case find (\(_, b) -> b == c) macrons of 
  Just (a, _) -> a
  Nothing -> return c

toMacron' :: Char -> Choice Char
toMacron' = toMacron . fromMacron

isVowel :: Char -> Bool
isVowel c = c `elem` "aiueo"

separator :: Char
separator = '$'

isSeparator :: Char -> Bool
isSeparator = (==) separator
