module Token.Romaji (
  chlst
, lookup
, many
, geminate
, longVowelize
, isSyllabicN
) where

import           Data.List (nub, sort)
import qualified Data.Map as M
import           Prelude hiding (lookup)

import           Token.Token (Token(..), (<$.>), unwrapToken, isRomajiToken)
import           Token.Misc (beMacron, isVowel)
import           Token.Internal (hRaw, kRaw)

chlst :: [Token]
chlst = nub $ sort $ concatMap (many . Romaji) $ map snd $ hRaw ++ kRaw

chmap :: M.Map String (String, String)
chmap = M.fromList $ zipWith helper hRaw kRaw
  where
    helper (h, hr) (k, kr) | hr /= kr  = error "bad data" -- never be here
                           | otherwise = (hr, (h, k))

lookup :: Token -> Maybe (Token, Token)
lookup r | isRomajiToken r = (\(h, k) -> return (Hiragana h, Katakana k)) =<< M.lookup (unwrapToken r) chmap
lookup _ = error "Romaji lookup: not romaji"

many :: Token -> [Token]
many r | isRomajiToken r = map Romaji $ many' $ unwrapToken r
  where
    -- Syllabic n
    many' "n"  = ["n", "m", "nn", "n-"]
    -- Particles mutation
    many' "ha" = ["ha", "wa"]
    many' "he" = ["he", "e"]
    many' "wo" = ["wo", "o"]
    -- default
    many' r    = [r]
many _ = error "Romaji many: not romaji"

-- chi -> tchi, ka -> kka .. a -> a
geminate :: Token -> Token
geminate r | isRomajiToken r = geminate' <$.> r
  where 
    geminate' s@('c':'h':_) = 't':s
    geminate' s@(c:_) | isVowel c = s    -- todo: report a warning
                      | otherwise = c:s
geminate _  = error "Romaji geminate: not romaji"

longVowelize :: Bool -> Token -> Token
longVowelize m r | isRomajiToken r = longVowelize' <$.> r 
  where
    longVowelize' s | not (isVowel (last s)) = s   -- todo: report a warning
                    | m                      = init s ++ [beMacron (last s)]
                    | otherwise              = s ++ [last s]  
longVowelize _ _ = error "Romaji longVowelize: not romaji"

isSyllabicN :: Token -> Bool
isSyllabicN n = n `elem` (many (Romaji "n"))
