module Parakeet.Linguistics.Misc (
  isKanji
, isChoonpu
, isMacron
, unMacron
, toMacron
, isVowel
, separator
, isSeparator
) where

import Data.Char (ord)
import Data.List (find)

isKanji :: Char -> Bool
isKanji = (\x -> (x >= 0x4e00 && x <= 0x9fbf) || x ==  0x3005) . ord
-- 0x3005 is kanji iteration mark

isChoonpu :: Char -> Bool 
isChoonpu = (==) 'ー'

macrons = [ ("āâ", 'a')
          , ("īî", 'i')
          , ("ūû", 'u')
          , ("ēê", 'e')
          , ("ōô", 'o')
          ]

isMacron :: Char -> Bool
isMacron c = c `elem` concatMap fst macrons

unMacron :: Char -> Char
unMacron c = case find (\(a, _) -> c `elem` a) macrons of 
  Just (_, b) -> b
  Nothing -> c

toMacron :: Char -> (Char, Char)
toMacron c = case find (\(_, b) -> b == c) macrons of 
  Just ([a1, a2], _) -> (a1, a2)
  Nothing -> (c, c)
  _ -> undefined -- never be here

isVowel :: Char -> Bool
isVowel c = c `elem` "aiueo"

separator :: Char
separator = '$'

isSeparator :: Char -> Bool
isSeparator = (==) separator
