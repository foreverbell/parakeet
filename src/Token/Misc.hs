module Token.Misc (
  isKanji
, isChoonpu
) where

import Data.Char (ord)

isKanji :: Char -> Bool    -- 漢字
isKanji = (\x -> x >= 0x4e00 && x <= 0x9fbf) . ord

isChoonpu :: Char -> Bool  -- 長音符
isChoonpu = (==) 'ー'
