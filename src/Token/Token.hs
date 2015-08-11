module Token.Token (
  Token(..)
, isKanji
, isChoonpu
) where

import           Control.Monad (mplus)
import           Data.Char (ord)
import           Data.Maybe (isJust)
import qualified Data.Map as M

data Token = Kanji String 
           | Hiragana String
           | Katakana String
           | Romaji String
           | Lit String
           deriving (Show, Eq)

isKanji :: Char -> Bool    -- 漢字
isKanji = (\x -> x >= 0x4e00 && x <= 0x9fbf) . ord

isChoonpu :: Char -> Bool  -- 長音符
isChoonpu = (==) 'ー'
