module Token.Katakana (
  lookup
, isNormal
, isSmall
, isSokuon
, isKatakana
) where

import           Control.Monad (liftM)
import           Data.Maybe (isJust)
import qualified Data.Map as M
import           Prelude hiding (lookup)

import           Token.Token
import           Token.Romaji (geminate, longVowel)
import           Token.Internal (kRaw)

chmap :: M.Map String String
chmap = M.fromList kRaw

lookup :: String -> Maybe Token
lookup [] = Nothing
lookup k | isSokuon (head k) = geminate `liftM` lookup (tail k)
         | isChoonpu (last k) = longVowel `liftM` lookup (init k)
         | otherwise = Romaji `liftM` M.lookup k chmap

isNormal :: Char -> Bool
isNormal = isJust . lookup . return

isSmall :: Char -> Bool
isSmall c = c `elem` ['ァ', 'ィ', 'ゥ', 'ェ', 'ォ', 'ッ', 'ャ', 'ュ', 'ョ', 'ヮ', 'ヵ', 'ヶ']
    -- [0x30a1, 0x30a3, 0x30a5, 0x30a7, 0x30a9, 0x30c3, 0x30e3, 0x30e5, 0x30e7, 0x30ee, 0x30f5, 0x30f6]

isSokuon :: Char -> Bool  -- 片仮名促音
isSokuon = (==) 'ッ'

isKatakana :: Char -> Bool
isKatakana c = (isNormal c) || (isSmall c)
