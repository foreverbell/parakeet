module Token.Hiragana (
  lookup
, isNormal
, isSmall
, isSokuon
, isHiragana
) where

import           Control.Monad (liftM)
import           Data.Maybe (isJust)
import qualified Data.Map as M
import           Prelude hiding (lookup)

import           Token.Token
import           Token.Romaji (geminate)
import           Token.Internal (hRaw)

chmap :: M.Map String String
chmap = M.fromList hRaw

lookup :: String -> Maybe Token
lookup [] = Nothing
lookup h | isSokuon (head h) = geminate `liftM` lookup (tail h)
         | otherwise         = Romaji `liftM` M.lookup h chmap

isNormal :: Char -> Bool
isNormal = isJust . lookup . return

isSmall :: Char -> Bool
isSmall c = c `elem` ['ぁ', 'ぃ', 'ぅ', 'ぇ', 'ぉ', 'っ', 'ゃ', 'ゅ', 'ょ', 'ゎ']
    -- [0x3041, 0x3043, 0x3045, 0x3047, 0x3049, 0x3063, 0x3083, 0x3085, 0x3087, 0x308e, 0x3095, 0x3096] 
    -- last two (3095, 3096) aren't commonly used in modern Japanese

isSokuon :: Char -> Bool  -- 平仮名促音
isSokuon = (==) 'っ'

isHiragana :: Char -> Bool
isHiragana c = (isNormal c) || (isSmall c)
