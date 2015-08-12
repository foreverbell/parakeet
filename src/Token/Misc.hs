module Token.Misc (
  isKanji
, isChoonpu
, isMacron
, noMacron
, beMacron
, isVowel
) where

import Data.Char (ord)
import Data.List (find)

isKanji :: Char -> Bool    -- 漢字
isKanji = (\x -> x >= 0x4e00 && x <= 0x9fbf) . ord

isChoonpu :: Char -> Bool  -- 長音符
isChoonpu = (==) 'ー'

macrons = [('ā', 'a'), ('ī', 'i'), ('ū', 'u'), ('ē', 'e'), ('ō', 'o')]

isMacron :: Char -> Bool
isMacron c = c `elem` (map fst macrons)
  
noMacron :: Char -> Char
noMacron c = case find (\(a, b) -> a == c) macrons of 
  Just (a, b) -> b
  Nothing -> c

beMacron :: Char -> Char
beMacron c = case find (\(a, b) -> b == c) macrons of 
  Just (a, b) -> a
  Nothing -> c

isVowel :: Char -> Bool
isVowel c = c `elem` "aiueo"

