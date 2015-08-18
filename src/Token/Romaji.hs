module Token.Romaji (
  chlst
, fromRomaji
, otherForms
, normalize
, sokuonize
, longVowelize
, isSyllabicN
) where

import           Control.Applicative ((<$>))
import           Data.List (nub, sort)
import qualified Data.Map as M

import           Token.Token (Token(..), (<$$>), unwrapToken, isRomajiToken)
import           Token.Misc (isMacron, toMacron, unMacron, isVowel)
import           Token.Internal (hRaw, kRaw)

chlst :: [Token]
chlst = nub $ sort $ concatMap (otherForms . Romaji . snd) $ hRaw ++ kRaw

chmap :: M.Map String (String, String)
chmap = M.fromList $ zipWith helper hRaw kRaw
  where
    helper (h, hr) (k, kr) | hr /= kr  = error "bad data" -- never be here
                           | otherwise = (hr, (h, k))

fromRomaji :: Token -> Maybe (Token, Token)
fromRomaji r | isRomajiToken r = (\(h, k) -> return (Hiragana h, Katakana k)) =<< M.lookup (unwrapToken r) chmap
fromRomaji _ = error "Romaji fromRomaji: not romaji"

otherForms :: Token -> [Token]
otherForms r | isRomajiToken r = map Romaji $ otherForms' $ unwrapToken r
  where
    -- Syllabic n
    otherForms' "n"  = ["n", "m", "nn", "n-", "n'"]
    -- Particles mutation
    otherForms' "ha" = ["ha", "wa"]
    otherForms' "he" = ["he", "e"]
    otherForms' "wo" = ["wo", "o"]
    -- default
    otherForms' r    = [r]
otherForms _ = error "Romaji otherForms: not romaji"

-- tchī -> [t, chi, i]
normalize :: Token -> [Token]
normalize r | isSyllabicN r = [r]
            | isRomajiToken r = let r' = unwrapToken r
                                in  if null r'
                                      then []
                                      else let (unS, next) = unSokuonize r'
                                               (unL, norm) = unLongVowelize next
                                           in Romaji <$> unS ++ [norm] ++ unL
  where
    unSokuonize r = if r == concatMap unwrapToken (sokuonize [Romaji (tail r)])
      then ([[head r]], tail r)
      else ([], r)
    unLongVowelize r = if isMacron l
                         then ([[k]], (b ++ [t]))
                         else ([], r)
                         where l = last r
                               b = init r
                               t = unMacron l
                               k | t == 'o' = 'u' -- ambiguous 'ō' -> ou 
                                 | otherwise = t
normalize _ = error "Romaji normalize: not romaji"

-- chi -> tchi, ka -> kka .. a -> a
sokuonize :: [Token] -> [Token]
sokuonize [] = []
sokuonize r | all isRomajiToken r = (sokuonize' <$$> head r) ++ tail r
  where 
    sokuonize' [] = []
    sokuonize' s@('c' : 'h' : _) = ["t", s]
    sokuonize' s@(c : _) | sokuonizable c = [[c], s]
                         | otherwise      = [s]
    sokuonizable c = c `notElem` "aiueonmrwy" -- ++ "gzdbh"
    -- https://en.wikipedia.org/wiki/Sokuon
sokuonize _  = error "Romaji sokuonize: not romaji"

longVowelize :: Bool -> [Token] -> [Token]
longVowelize _ [] = []
longVowelize m r | all isRomajiToken r = init r ++ (longVowelize' <$$> (last r))
  where
    longVowelize' [] = []
    longVowelize' s | not (isVowel (last s)) = [s]
                    | m                      = [init s ++ [toMacron (last s)]]
                    | otherwise              = [s, [last s]]  
longVowelize _ _ = error "Romaji longVowelize: not romaji"

isSyllabicN :: Token -> Bool
isSyllabicN n = n `elem` otherForms (Romaji "n")
