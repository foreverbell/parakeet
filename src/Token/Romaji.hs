module Token.Romaji (
  chlst
, fromRomaji
, otherForms
, normSyllabicN
, unSokuonize
, unLongVowelize
, cut
, sokuonize
, longVowelize
, isSyllabicN
) where

import           Data.List (nub, sort)
import qualified Data.Map as M
import           Data.Maybe (maybeToList)

import           Token.Token (wrap, unwrap, Hiragana, Katakana, Romaji, (<**>), (<$$>))
import           Token.Misc (isMacron, toMacron, unMacron, isVowel)
import           Token.Internal (hRaw, kRaw)
import           Monad.Choice (Choice, fromList, toList)

chlst :: [Romaji]
chlst = nub $ sort $ concatMap (toList . otherForms . wrap . snd) $ hRaw ++ kRaw

chmap :: M.Map String (String, String)
chmap = M.fromList $ zipWith helper hRaw kRaw
  where
    helper (h, hr) (k, kr) | hr /= kr  = error "bad data" -- never be here
                           | otherwise = (hr, (h, k))

fromRomaji :: Romaji -> Maybe (Hiragana, Katakana)
fromRomaji r = (\(h, k) -> return (wrap h, wrap k)) =<< M.lookup (unwrap r) chmap

otherForms :: Romaji -> Choice Romaji
otherForms r = otherForms' <$$> r
  where
    -- Syllabic n
    otherForms' "n"  = fromList ["n", "m", "nn", "n-", "n'"]
    -- Particles mutation
    otherForms' "ha" = fromList ["ha", "wa"]
    otherForms' "he" = fromList ["he", "e"]
    otherForms' "wo" = fromList ["wo", "o"]
    -- Otherwise
    otherForms' r    = return r

normSyllabicN :: Romaji -> Romaji
normSyllabicN r = if isSyllabicN r
    then return . head <**> r
    else r

unSokuonize :: Romaji -> Choice (Maybe Romaji, Romaji)
unSokuonize r
  | null (unwrap r) = return (Nothing, r)
  | mconcat (sokuonize [tail <**> r]) == r
      = return (Just (return . head <**> r), tail <**> r)
  | otherwise = return (Nothing, r)

unLongVowelize :: Romaji -> Choice (Maybe Romaji, Romaji)
unLongVowelize r 
  | null (unwrap r) = return (Nothing, r)
  | isMacron lastOne = do
      to <- lastTo
      return (Just (wrap [to]), wrap $ exceptLast ++ [lastDesugar])
--  | length (unwrap r) >= 2 && isVowel lastOne && isVowel sndLast = return (Just (wrap [lastOne]), wrap exceptLast)
  | otherwise = return (Nothing, r)
  where
    lastOne = last $ unwrap r
    -- sndLast = last $ exceptLast
    exceptLast = init $ unwrap r
    lastDesugar = unMacron lastOne
    lastTo | lastDesugar == 'o' = fromList ['u', 'o']  -- ambiguous 'ō' -> ou
           | otherwise          = return lastDesugar

-- divide a Romaji into different parts, e.g. tchī -> [t, chi, i]
cut :: Romaji -> Choice [Romaji]
cut r = do 
  (sokuonPart, next) <- unSokuonize r
  (longVowelPart, normalized) <- unLongVowelize next
  return $ maybeToList sokuonPart ++ [normalized] ++ maybeToList longVowelPart

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
                    | m                      = [init s ++ [fst $ toMacron (last s)]]
                    | otherwise              = [s, [last s]] 

isSyllabicN :: Romaji -> Bool
isSyllabicN n = n `elem` toList (otherForms (wrap "n"))
