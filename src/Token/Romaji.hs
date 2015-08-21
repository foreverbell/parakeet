module Token.Romaji (
  chlst
, fromRomaji
, otherForms
, normalize
, sokuonize
, longVowelize
, isSyllabicN
) where

import           Data.List (nub, sort)
import qualified Data.Map as M

import           Token.Token (wrap, unwrap, Hiragana, Katakana, Romaji, (<**>), (<$$>))
import           Token.Misc (isMacron, toMacron, unMacron, isVowel)
import           Token.Internal (hRaw, kRaw)

chlst :: [Romaji]
chlst = nub $ sort $ concatMap (otherForms . wrap . snd) $ hRaw ++ kRaw

chmap :: M.Map String (String, String)
chmap = M.fromList $ zipWith helper hRaw kRaw
  where
    helper (h, hr) (k, kr) | hr /= kr  = error "bad data" -- never be here
                           | otherwise = (hr, (h, k))

fromRomaji :: Romaji -> Maybe (Hiragana, Katakana)
fromRomaji r = (\(h, k) -> return (wrap h, wrap k)) =<< M.lookup (unwrap r) chmap

otherForms :: Romaji -> [Romaji]
otherForms r = otherForms' <$$> r
  where
    -- Syllabic n
    otherForms' "n"  = ["n", "m", "nn", "n-", "n'"]
    -- Particles mutation
    otherForms' "ha" = ["ha", "wa"]
    otherForms' "he" = ["he", "e"]
    otherForms' "wo" = ["wo", "o"]
    -- default
    otherForms' r    = [r]

-- tchī -> [t, chi, i]
normalize :: Romaji -> [Romaji]
normalize r = if isSyllabicN r
                then [(return . head) <**> r]
                else normalize' <$$> r 
                where 
                  normalize' [] = []
                  normalize' r  = let (unS, next) = unSokuonize r
                                      (unL, norm) = unLongVowelize next
                                   in unS ++ [norm] ++ unL
                  unSokuonize r = if r == concatMap unwrap (sokuonize [wrap (tail r)])
                                    then ([[head r]], tail r)
                                    else ([], r)
                  unLongVowelize r = if isMacron l
                                       then ([[k]], b ++ [t])
                                       else ([], r)
                                       where l = last r
                                             b = init r
                                             t = unMacron l
                                             k | t == 'o' = 'u' -- ambiguous 'ō' -> ou 
                                               | otherwise = t

-- chi -> tchi, ka -> kka .. a -> a
sokuonize :: [Romaji] -> [Romaji]
sokuonize [] = []
sokuonize r = (sokuonize' <$$> head r) ++ tail r
  where 
    sokuonize' [] = []
    sokuonize' s@('c' : 'h' : _) = ["t", s]
    sokuonize' s@(c : _) | sokuonizable c = [[c], s]
                         | otherwise      = [s]
    sokuonizable c = c `notElem` "aiueonmrwy" -- ++ "gzdbh"
    -- https://en.wikipedia.org/wiki/Sokuon

longVowelize :: Bool -> [Romaji] -> [Romaji]
longVowelize _ [] = []
longVowelize m r = init r ++ (longVowelize' <$$> last r)
  where
    longVowelize' [] = []
    longVowelize' s | not (isVowel (last s)) = [s]
                    | m                      = [init s ++ [toMacron (last s)]]
                    | otherwise              = [s, [last s]] 

isSyllabicN :: Romaji -> Bool
isSyllabicN n = n `elem` otherForms (wrap "n")
