module Token.Romaji (
  chlst
, fromRomaji
, many
, normalize
, sokuonize
, longVowelize
, isSyllabicN
) where

import           Data.List (nub, sort)
import qualified Data.Map as M

import           Token.Token (Token(..), (<$.>), unwrapToken, isRomajiToken)
import           Token.Misc (isMacron, beMacron, unMacron, isVowel)
import           Token.Internal (hRaw, kRaw)

chlst :: [Token]
chlst = nub $ sort $ concatMap (many . Romaji) $ map snd $ hRaw ++ kRaw

chmap :: M.Map String (String, String)
chmap = M.fromList $ zipWith helper hRaw kRaw
  where
    helper (h, hr) (k, kr) | hr /= kr  = error "bad data" -- never be here
                           | otherwise = (hr, (h, k))

fromRomaji :: Token -> Maybe (Token, Token)
fromRomaji r | isRomajiToken r = (\(h, k) -> return (Hiragana h, Katakana k)) =<< M.lookup (unwrapToken r) chmap
fromRomaji _ = error "Romaji fromRomaji: not romaji"

many :: Token -> [Token]
many r | isRomajiToken r = map Romaji $ many' $ unwrapToken r
  where
    -- Syllabic n
    many' "n"  = ["n", "m", "nn", "n-", "n'"]
    -- Particles mutation
    many' "ha" = ["ha", "wa"]
    many' "he" = ["he", "e"]
    many' "wo" = ["wo", "o"]
    -- default
    many' r    = [r]
many _ = error "Romaji many: not romaji"

-- tchī -> [t, chi, i]
normalize :: Token -> [Token]
normalize r | isSyllabicN r = [r]
            | isRomajiToken r = if null (unwrapToken r) then []
                      else let (unS, next) = unSokuonize r
                               (unL, norm) = unLongVowelize next
                           in unS ++ [norm] ++ unL
  where
    unSokuonize r = if sokuonize (tail <$.> r) == r
      then ([f (unwrapToken r)], tail <$.> r)
      else ([], r)
      where f ('t':'c':'h':_) = Romaji "t"
            f (c:_) = Romaji [c]
    unLongVowelize r = f (unwrapToken r)
      where f r = if (isMacron l)
                    then ([Romaji [k]], Romaji (b ++ [t]))
                    else ([], Romaji r)
                    where l = last r
                          b = init r
                          t = unMacron l
                          k | t == 'o' = 'u' -- ambiguous 'ō' -> ou 
                            | otherwise = t
normalize _ = error "Romaji normalize: not romaji"

-- chi -> tchi, ka -> kka .. a -> a
sokuonize :: Token -> Token
sokuonize r | isRomajiToken r = sokuonize' <$.> r
  where 
    sokuonize' [] = []
    sokuonize' s@('c':'h':_) = 't':s
    sokuonize' s@(c:_) | sokuonizable c = c:s
                       | otherwise      = s
    sokuonizable c = c `notElem` "aiueonmrwy" -- ++ "gzdbh"
    -- https://en.wikipedia.org/wiki/Sokuon
sokuonize _  = error "Romaji sokuonize: not romaji"

longVowelize :: Bool -> Token -> Token
longVowelize m r | isRomajiToken r = longVowelize' <$.> r 
  where
    longVowelize' [] = []
    longVowelize' s | not (isVowel (last s)) = s
                    | m                      = init s ++ [beMacron (last s)]
                    | otherwise              = s ++ [last s]  
longVowelize _ _ = error "Romaji longVowelize: not romaji"

isSyllabicN :: Token -> Bool
isSyllabicN n = n `elem` (many (Romaji "n"))
