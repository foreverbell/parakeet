module Token.Misc (
  isKanji
, isChoonpu
, isMacron
, unMacron
, toMacron
, isVowel
) where

import Data.Char (ord)
import Data.List (find)

isKanji :: Char -> Bool
isKanji = (\x -> (x >= 0x4e00 && x <= 0x9fbf) || x ==  0x3005) . ord
-- 0x3005 is kanji iteration mark

isChoonpu :: Char -> Bool 
isChoonpu = (==) 'ー'

macrons = [('ā', 'a'), ('ī', 'i'), ('ū', 'u'), ('ē', 'e'), ('ō', 'o')]

isMacron :: Char -> Bool
isMacron c = c `elem` map fst macrons
  
unMacron :: Char -> Char
unMacron c = case find (\(a, _) -> a == c) macrons of 
  Just (_, b) -> b
  Nothing -> c

toMacron :: Char -> Char
toMacron c = case find (\(_, b) -> b == c) macrons of 
  Just (a, _) -> a
  Nothing -> c

isVowel :: Char -> Bool
isVowel c = c `elem` "aiueo"
