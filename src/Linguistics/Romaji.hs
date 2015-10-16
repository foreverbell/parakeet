module Linguistics.Romaji (
  chList
, toKana
, otherForms
, normSyllabicN
, unSokuonize
, unLongVowelize
, cut
, sokuonize
, longVowelize
, isSyllabicN
) where

import           Data.Tuple (swap)
import           Data.List (sort, group)
import qualified Data.Map as M
import           Data.Maybe (maybeToList, fromJust, fromMaybe)
import           Control.Arrow (second)
import           Control.Monad (mzero)

import           Linguistics.Lexeme (wrap, unwrap, Hiragana, Katakana, Romaji, (<**>), (<$$>))
import           Linguistics.Misc (isMacron, toMacron, unMacron, isVowel)
import           Linguistics.Internal (hRaw, kRaw)
import           Monad.Choice (Choice, fromList, toList)

chList :: [Romaji]
chList = nub' $ concatMap (toList . otherForms . wrap . snd) $ hRaw ++ kRaw
  where nub' = map head . group . sort

buildMap :: [(String, String)] -> M.Map String (Choice String)
buildMap raw = M.fromList $ map toChoice (raw ++ raw')
  where 
    swapped = map swap raw
    toChoice (k, r) = (r, return k)
    raw' = concatMap f (tail otherList) 
      where 
        f (r, rs) = foldl g [] rs where
          kana = fromJust $ lookup r swapped
          g xs cur = case lookup cur swapped of
            Nothing -> (kana, cur) : xs
            _ -> xs

hMap :: M.Map String (Choice String)
hMap = buildMap hRaw

kMap :: M.Map String (Choice String)
kMap = buildMap kRaw

toKana :: Romaji -> (Choice Hiragana, Choice Katakana)
toKana r = (lookup hMap, lookup kMap)
  where
    lookup m = case M.lookup (unwrap r) m of
      Just ch -> wrap <$> ch
      Nothing -> mzero

otherList :: [(String, [String])]
otherList = [ ("n",  ["n", "m", "nn", "n-", "n'"]) -- Syllabic n
            , ("ha", ["ha", "wa"])                 -- Particles mutation
            , ("he", ["he", "e"])
            , ("wo", ["wo", "o"])
            , ("di", ["di", "ji", "dji"])          -- Ambiguous ji & zu
            , ("du", ["du", "zu", "dzu"])
            ]

otherMap :: M.Map String (Choice String)
otherMap = M.fromList $ map (second fromList) otherList

otherForms :: Romaji -> Choice Romaji
otherForms r = go <$$> r
  where go r = fromMaybe (return r) (M.lookup r otherMap)

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
