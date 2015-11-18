module Parakeet.Linguistics.Romaji (
  chList
, toKana
, otherForms
, dakutenize
, unDakutenize
, isSyllabicN
, normSyllabicN
, sokuonize
, unSokuonize
, longVowelize
, unLongVowelize
, factor
) where

import           Data.Tuple (swap)
import           Data.List (sort, group)
import qualified Data.Map as M
import           Data.Maybe (maybeToList, fromJust, fromMaybe, isNothing)
import           Control.Arrow (second)
import           Control.Monad (mzero, guard)
import           Control.Monad.Choice (Choice, fromList, toList)

import           Parakeet.Linguistics.Lexeme (wrap, unwrap, toRLV, Hiragana, Katakana, Romaji, (<**>), (<$$>))
import           Parakeet.Linguistics.Misc (isMacron, toMacron, unMacron, isVowel)
import           Parakeet.Linguistics.Internal (hRaw, kRaw)

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

buildDakutenPairs :: [(Int, Int)] -> [(String, String)]
buildDakutenPairs = extend . concatMap convert 
  where
    nline = replicate 14 5 ++ [1] ++ replicate 11 3 :: [Int]
    prefix = zipWith (+) (0 : prefix) nline
    convert (l1, l2) = map (\x -> (snd (hRaw !! (offset1 + x)), snd (hRaw !! (offset2 + x)))) [0 .. count - 1]
      where 
        count = nline !! l1
        offset1 = prefix !! (l1 - 1)
        offset2 = prefix !! (l2 - 1)
    extend pairs = pairs ++ concatMap f pairs
      where f (s1, s2) = do
              x <- unwrap <$> toList (otherForms (wrap s1))
              guard $ isNothing (lookup x pairs)
              return (x, s2)

dakutenMap :: M.Map String String
dakutenMap = M.fromList $ buildDakutenPairs (dakutenPair1 ++ map swap dakutenPair3)

unDakutenMap :: M.Map String String
unDakutenMap = M.fromList $ buildDakutenPairs $ map swap (dakutenPair1 ++ dakutenPair2)

dakutenPair1 = [(1, 2), (3, 4), (5, 6), (8, 9), (15, 16), (17, 18), (21, 22)] :: [(Int, Int)] -- (normal, dakuten)
dakutenPair2 = [(8, 10), (21, 23)] :: [(Int, Int)] -- (normal, han-dakuten)
dakutenPair3 = [(9, 10), (22, 23)] :: [(Int, Int)] -- (dakuten, han-dakuten)

dakutenize :: Romaji -> Romaji
dakutenize r = lookup <**> r
  where lookup x = fromMaybe x (M.lookup x dakutenMap)

unDakutenize :: Romaji -> Romaji
unDakutenize r = lookup <**> r
  where lookup x = fromMaybe x (M.lookup x unDakutenMap)

isSyllabicN :: Romaji -> Bool
isSyllabicN n = n `elem` toList (otherForms (wrap "n"))

-- | Normalize syllabic n (take the first alphabet).
normSyllabicN :: Romaji -> Romaji
normSyllabicN r = if isSyllabicN r
    then return . head <**> r
    else r

-- | Sokuonize a factorized romaji, chi -> tchi, ka -> kka, a -> a.
sokuonize :: [Romaji] -> [Romaji]
sokuonize [] = []
sokuonize r = (sokuonize' <$$> head r) ++ tail r
  where 
    sokuonize' [] = []
    sokuonize' s@('c':'h':_) = ["t", s]
    sokuonize' s@(c:_) | sokuonizable c = [[c], s]
                         | otherwise      = [s]
    sokuonizable c = c `notElem` "aiueonmrwy" -- ++ "gzdbh"
    -- https://en.wikipedia.org/wiki/Sokuon

unSokuonize :: Romaji -> Choice (Maybe Romaji, Romaji)
unSokuonize r
  | null (unwrap r) = return (Nothing, r)
  | mconcat (sokuonize [tail <**> r]) == r
      = return (Just (return . head <**> r), tail <**> r)
  | otherwise = return (Nothing, r)

-- | Long vowelize a factorized romaji.
longVowelize :: Bool -> [Romaji] -> [Romaji]
longVowelize _ [] = []
longVowelize m r = init r ++ (longVowelize' <$$> last r)
  where
    longVowelize' [] = []
    longVowelize' s | not (isVowel (last s)) = [s]
                    | m                      = [init s ++ [fst $ toMacron (last s)]]
                    | otherwise              = [s, [last s]] 

unLongVowelize :: Romaji -> Choice (Maybe Romaji, Romaji)
unLongVowelize r 
  | null (unwrap r) = return (Nothing, r)
  | isMacron lastOne = do
      to <- lastTo
      return (Just (wrap [to]), wrap $ exceptLast ++ [lastDesugar])
  | otherwise = return (Nothing, r)
  where
    lastOne = last $ unwrap r
    -- sndLast = last $ exceptLast
    exceptLast = init $ unwrap r
    lastDesugar = unMacron lastOne
    lastTo | lastDesugar == 'o' = fromList ['u', 'o']  -- ambiguous 'ō' -> ou
           | otherwise          = return lastDesugar

-- | Factorize a romaji with possible sokuon & macron into different parts, e.g. tchī -> [t, chi, i].
factor :: Romaji -> Choice [Romaji]
factor r = do 
  (sokuonPart, next) <- unSokuonize r
  (longVowelPart, normalized') <- unLongVowelize next
  let normalized = if null longVowelPart then normalized' else toRLV normalized'
  return $ maybeToList sokuonPart ++ [normalized] ++ maybeToList longVowelPart

