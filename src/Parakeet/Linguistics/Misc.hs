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
isKanji = (\x -> (x >= 0x4e00 && x <= 0x9fbf) || x ==  0x3005) . ord -- 0x3005 is kanji iteration mark

isChoonpu :: Char -> Bool 
isChoonpu = (==) 'ー'

macrons :: [((Char, Char), Char)]
macrons = [ (('ā', 'â'), 'a')
          , (('ī', 'î'), 'i')
          , (('ū', 'û'), 'u')
          , (('ē', 'ê'), 'e')
          , (('ō', 'ô'), 'o')
          ]

isMacron :: Char -> Bool
isMacron c = c `elem` concatMap ((\(a, b) -> [a, b]) . fst) macrons

unMacron :: Char -> Char
unMacron c = case find (\((a1, a2), _) -> c == a1 || c == a2) macrons of 
  Just (_, b) -> b
  Nothing -> c

toMacron :: Char -> (Char, Char)
toMacron c = case find (\(_, b) -> b == c) macrons of 
  Just (a, _) -> a
  Nothing -> (c, c)

isVowel :: Char -> Bool
isVowel c = c `elem` "aiueo"

separator :: Char
separator = '$'

isSeparator :: Char -> Bool
isSeparator = (==) separator
