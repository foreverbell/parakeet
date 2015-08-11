module Token.Romaji (
  lookup
, geminate
, longVowel
) where

import qualified Data.Map as M
import           Prelude hiding (lookup)

import           Token.Token
import           Token.Internal (hRaw, kRaw)

chmap :: M.Map String (String, String)
chmap = M.fromList $ zipWith helper hRaw kRaw
  where
    helper (h, hr) (k, kr) | hr /= kr  = error "bad data" -- never be here
                           | otherwise = (hr, (h, k))

lookup :: String -> Maybe (Token, Token)
lookup r = (\(h, k) -> return (Hiragana h, Katakana k)) =<< (M.lookup r chmap)

-- chi -> tchi, ka -> kka
geminate :: Token -> Token
geminate (Romaji s@('c':'h':_)) = Romaji $ 't':s
geminate (Romaji s@(c:_))       = Romaji $ c:s
geminate _                      = error "Romaji geminate: not romaji"

longVowel :: Token -> Token
longVowel (Romaji s) = Romaji $ s ++ [last s]
longVowel _          = error "Romaji long vowel: not romaji"
